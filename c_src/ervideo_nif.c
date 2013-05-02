#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <malloc.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/mman.h>
#include <sys/ioctl.h>
#include <asm/types.h>
#include <linux/videodev2.h>
#include <stdbool.h>

#include "lodepng.h"

#include "erl_nif.h"

#define ATOM(x) atm_##x
#define DECL_ATOM(x)  ERL_NIF_TERM atm_##x = 0
#define LOAD_ATOM(x)  atm_##x = enif_make_atom(env, #x)
#define LOAD_ATOM_STRING(x,str) atm_##x = enif_make_atom(env, str)
#define IS_ERVIDEO(env,term,tuple,arity,obj) (enif_get_tuple((env), (term), &(arity), (const ERL_NIF_TERM **) &(tuple)) && \
  (arity) == 3 && (tuple)[0] == ATOM(ervideo) && enif_get_resource(env, (tuple)[2], erv_video, (void **) &(obj)) && \
  (obj)->fd >= 0)

DECL_ATOM(ervideo);

ErlNifResourceType* erv_video;

#define CLEAR(x)  memset(&(x), 0, sizeof(x))
#define MAX(a,b) ((a) > (b) ? (a) : (b))
#define CLIP(x) ( (x) > 0xFF ? 0xFF : ( (x) < 0x00 ? 0x00 : (x) ) )
#define frame_dup(obj,src,dst) memcpy(dst, src, obj->width * obj->height * 2 * sizeof(unsigned char))

/* structs {{{ */
typedef enum {
  IO_METHOD_READ,
  IO_METHOD_MMAP,
  IO_METHOD_USERPTR
} io_method;

struct buffer {
  void *start;
  size_t length;
};

typedef struct {
  ErlNifEnv *env;
  io_method method;
  int fd;
  struct buffer *buffers;
  unsigned int n_buffers;
  unsigned int width;
  unsigned int height;
  bool enabled;
} video_object_t;
/* }}} */

void
free_buffers(struct buffer *buffers, unsigned int n_buffers, io_method meth) {
  int i;

  if (buffers != NULL) {
    if (meth == IO_METHOD_READ) {
      free(buffers[0].start);
    }
    if (meth == IO_METHOD_USERPTR) {
      for (i = 0; i < n_buffers; ++i) {
        free(buffers[i].start);
      }
    }
    else {
      for (i = 0; i < n_buffers; ++i) {
        munmap(buffers[i].start, buffers[i].length);
      }
    }
    free(buffers);
  }
}

static
void
erv_video_dtor(ErlNifEnv *env, video_object_t *obj) {
  free_buffers(obj->buffers, obj->n_buffers, obj->method);
  enif_free_env(obj->env);
}

static
int
xioctl(int fd, int request, void* argp) {
  int r;

  do r = ioctl(fd, request, argp);
  while (r == -1 && errno == EINTR);

  return r;
}

bool
auto_io_method(int fd, struct v4l2_capability *cap, struct v4l2_requestbuffers *req, io_method *m) {
  if (cap->capabilities & V4L2_CAP_STREAMING) {
    CLEAR(*req);

    req->count = 4;
    req->type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    req->memory = V4L2_MEMORY_MMAP;

    if (xioctl(fd, VIDIOC_REQBUFS, req) == -1) {
      CLEAR(*req);

      req->count = 4;
      req->type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
      req->memory = V4L2_MEMORY_USERPTR;

      if (xioctl(fd, VIDIOC_REQBUFS, req) == -1) {
        return false;
      }
      else {
        *m = IO_METHOD_USERPTR;
      }
    }
    else {
      *m = IO_METHOD_MMAP;
    }
  }
  else if (cap->capabilities & V4L2_CAP_READWRITE) {
    *m = IO_METHOD_READ;
  }
  else {
    return false;
  }

  return true;
}

static
void
YUV422toRGB(int width, int height, unsigned char *src, unsigned char *dst)
{
  int line, column;
  unsigned char *py, *pu, *pv;
  unsigned char *tmp = dst;

  py = src;
  pu = src + 1;
  pv = src + 3;

  for (line = 0; line < height; ++line) {
    for (column = 0; column < width; ++column) {
      *tmp++ = CLIP((double)*py + 1.402*((double)*pv-128.0));
      *tmp++ = CLIP((double)*py - 0.344*((double)*pu-128.0) - 0.714*((double)*pv-128.0));
      *tmp++ = CLIP((double)*py + 1.772*((double)*pu-128.0));

      py += 2;

      if ((column & 1) == 1) {
        pu += 4;
        pv += 4;
      }
    }
  }
}

static
int
YUV422toPNG(int width, int height, unsigned char *src, unsigned char **dst, size_t *len) {
  unsigned char *rgb_buffer = malloc(width * height * 3 * sizeof(unsigned char));

  if (rgb_buffer == NULL) {
    return -1;
  }

  YUV422toRGB(width, height, src, rgb_buffer);
  if (lodepng_encode24(dst, len, rgb_buffer, width, height)) {
    free(rgb_buffer);
    return -1;
  }

  free(rgb_buffer);
  return 0;
}

int
capture_on(video_object_t *obj) {
  unsigned int i;
  enum v4l2_buf_type type;

  if (obj->enabled) {
    return 0;
  }

  switch (obj->method) {
    case IO_METHOD_READ: break;

    case IO_METHOD_MMAP:
      for (i = 0; i < obj->n_buffers; ++i) {
        struct v4l2_buffer buf; CLEAR(buf);

        buf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
        buf.memory = V4L2_MEMORY_MMAP;
        buf.index = i;

        if (xioctl(obj->fd, VIDIOC_QBUF, &buf) == -1) {
          return -1;
        }
      }

      type = V4L2_BUF_TYPE_VIDEO_CAPTURE;

      if (xioctl(obj->fd, VIDIOC_STREAMON, &type) == -1) {
        return -2;
      }
      break;
    case IO_METHOD_USERPTR:
      for (i = 0; i < obj->n_buffers; ++i) {
        struct v4l2_buffer buf; CLEAR(buf);

        buf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
        buf.memory = V4L2_MEMORY_USERPTR;
        buf.index = i;
        buf.m.userptr = (unsigned long) obj->buffers[i].start;
        buf.length  = obj->buffers[i].length;

        if (xioctl(obj->fd, VIDIOC_QBUF, &buf) == -1) {
          return -1;
        }
      }

      type = V4L2_BUF_TYPE_VIDEO_CAPTURE;

      if (xioctl(obj->fd, VIDIOC_STREAMON, &type)) {
        return -2;
      }
      break;
  }

  obj->enabled = true;
  return 0;
}

int
capture_off(video_object_t *obj) {
  enum v4l2_buf_type type;

  if (!obj->enabled) {
    return 0;
  }

  switch (obj->method) {
    case IO_METHOD_READ: break;

    case IO_METHOD_MMAP:
    case IO_METHOD_USERPTR:
      type = V4L2_BUF_TYPE_VIDEO_CAPTURE;

      if (xioctl(obj->fd, VIDIOC_STREAMOFF, &type) == -1) {
        return -1;
      }
      break;
  }

  obj->enabled = false;
  return 0;
}

int
frame_read(video_object_t *obj, unsigned char **bbuf) {
  struct v4l2_buffer buf;
  unsigned int i;

  switch (obj->method) {
    case IO_METHOD_READ:
      if (read(obj->fd, obj->buffers[0].start, obj->buffers[0].length) == -1) {
        switch (errno) {
          case EAGAIN:
            return 0;
          case EIO:
          default:
            return -1;
        }
      }

      *bbuf = obj->buffers[0].start;
      return 1;

    case IO_METHOD_MMAP:
      CLEAR(buf);

      buf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
      buf.memory = V4L2_MEMORY_MMAP;

      if (xioctl(obj->fd, VIDIOC_DQBUF, &buf) == -1) {
        switch (errno) {
          case EAGAIN:
            return 0;
          case EIO:
          default:
            return -2;
        }
      }

      if (buf.index >= obj->n_buffers) {
        return -3;
      }

      *bbuf = obj->buffers[buf.index].start;

      if (xioctl(obj->fd, VIDIOC_QBUF, &buf) == -1) {
        return -4;
      }
      return 1;

    case IO_METHOD_USERPTR:
      CLEAR(buf);

      buf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
      buf.memory = V4L2_MEMORY_USERPTR;

      if (xioctl(obj->fd, VIDIOC_DQBUF, &buf) == -1) {
        switch (errno) {
          case EAGAIN:
            return 0;
          case EIO:
          default:
            return -2;
        }
      }

      for (i = 0; i < obj->n_buffers; ++i) {
        if (buf.m.userptr == (unsigned long) obj->buffers[i].start && buf.length == obj->buffers[i].length) {
          break;
        }
      }

      if (i >= obj->n_buffers) {
        return -3;
      }

      *bbuf = (unsigned char *) buf.m.userptr;

      if (xioctl(obj->fd, VIDIOC_QBUF, &buf) == -1) {
        return -4;
      }
      return 1;
  }
  return -5;
}

#define _ERROR(x) enif_make_tuple(env, 2, enif_make_atom(env, "error"), enif_make_atom(env, #x))
#define ERROR(x)  (close(fd), _ERROR(x))

ERL_NIF_TERM
video_open(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  int width, height, fd;
  char type[8];
  char path[4096];
  io_method method;
  video_object_t *obj;
  ERL_NIF_TERM r;
  struct stat st;
  struct buffer *buffers;
  unsigned int n_buffers;
  struct v4l2_requestbuffers req;
  unsigned int page_size;
  bool already_req = false;

  struct v4l2_capability cap;
  struct v4l2_cropcap cropcap;
  struct v4l2_crop crop;
  struct v4l2_format fmt;
  unsigned int min;

  unsigned int n;

  if (enif_get_uint(env, argv[0], &n)) {
    sprintf(path, "/dev/video%d", n);
  }
  else if (enif_get_string(env, argv[0], path, 4096, ERL_NIF_LATIN1) < 1) {
    return enif_make_badarg(env);
  }

  if (!enif_get_int(env, argv[1], &width) ||
      !enif_get_int(env, argv[2], &height) ||
      enif_get_atom(env, argv[3], type, 8, ERL_NIF_LATIN1) < 1) {
    return enif_make_badarg(env);
  }

  if (stat(path, &st) == -1) {
    return _ERROR(enoent);
  }

  if (!S_ISCHR(st.st_mode)) {
    return _ERROR(enotchar);
  }

  fd = open(path, O_RDWR | O_NONBLOCK, 0);

  if (fd == -1) {
    return _ERROR(eisdir);
  }

  if (xioctl(fd, VIDIOC_QUERYCAP, &cap) == -1) {
    return ERROR(nov4l2);
  }

  if (!(cap.capabilities & V4L2_CAP_VIDEO_CAPTURE)) {
    return ERROR(nocapture);
  }

  CLEAR(cropcap);

  cropcap.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;

  if (xioctl(fd, VIDIOC_CROPCAP, &cropcap) == 0) {
    crop.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    crop.c = cropcap.defrect;

    xioctl(fd, VIDIOC_S_CROP, &crop); /* errors ignored */
  }

  CLEAR(fmt);

  fmt.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  fmt.fmt.pix.width = width;
  fmt.fmt.pix.height = height;
  fmt.fmt.pix.pixelformat = V4L2_PIX_FMT_YUYV;
  fmt.fmt.pix.field = V4L2_FIELD_INTERLACED;

  if (xioctl(fd, VIDIOC_S_FMT, &fmt) == -1) {
    return ERROR(vidioc_s_fmt);
  }

  if (fmt.fmt.pix.pixelformat != V4L2_PIX_FMT_YUYV) {
    return ERROR(yuyv_unsupported);
  }

  if (fmt.fmt.pix.width != width) {
    width = fmt.fmt.pix.width;
  }
  if (fmt.fmt.pix.height != height) {
    height = fmt.fmt.pix.height;
  }

  min = fmt.fmt.pix.width * 2;
  if (fmt.fmt.pix.bytesperline < min) {
    fmt.fmt.pix.bytesperline = min;
  }
  min = fmt.fmt.pix.bytesperline * fmt.fmt.pix.height;
  if (fmt.fmt.pix.sizeimage < min) {
    fmt.fmt.pix.sizeimage = min;
  }

  if (!strcmp(type, "auto")) {
    if (!auto_io_method(fd, &cap, &req, &method)) {
      return ERROR(no_io_methods);
    }
    already_req = true;
  }
  else if (!strcmp(type, "read")) {
    method = IO_METHOD_READ;
    if (!(cap.capabilities & V4L2_CAP_READWRITE)) {
      return ERROR(no_read);
    }
  }
  else if (!strcmp(type, "mmap")) {
    method = IO_METHOD_MMAP;
    if (!(cap.capabilities & V4L2_CAP_STREAMING)) {
      return ERROR(no_mmap);
    }
  }
  else if (!strcmp(type, "userptr")) {
    method = IO_METHOD_USERPTR;
    if (!(cap.capabilities & V4L2_CAP_STREAMING)) {
      return ERROR(no_userptr);
    }
  }
  else {
    close(fd);
    return enif_make_badarg(env);
  }

  switch (method) {
    case IO_METHOD_READ:
      buffers = calloc(1, sizeof(*buffers));

      if (!buffers) {
        return ERROR(out_of_memory);
      }

      buffers[0].length = fmt.fmt.pix.sizeimage;
      buffers[0].start = malloc(fmt.fmt.pix.sizeimage);
      n_buffers = 1;

      if (!buffers[0].start) {
        free_buffers(buffers, n_buffers, method);
        return ERROR(out_of_memory);
      }
      break;
    case IO_METHOD_MMAP:
      if (!already_req) {
        CLEAR(req);

        req.count = 4;
        req.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
        req.memory = V4L2_MEMORY_MMAP;

        if (xioctl(fd, VIDIOC_REQBUFS, &req) == -1) {
          if (errno == EINVAL) {
            return ERROR(no_mmap);
          }
          return ERROR(vidioc_reqbufs);
        }
      }

      if (req.count < 2) {
        return ERROR(insufficient_buffer);
      }

      buffers = calloc(req.count, sizeof(*buffers));

      if (!buffers) {
        return ERROR(out_of_memory);
      }

      for (n_buffers = 0; n_buffers < req.count; ++n_buffers) {
        struct v4l2_buffer buf; CLEAR(buf);

        buf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
        buf.memory = V4L2_MEMORY_MMAP;
        buf.index = n_buffers;

        if (xioctl(fd, VIDIOC_QUERYBUF, &buf) == -1) {
          free_buffers(buffers, n_buffers, method);
          return ERROR(vidioc_querybuf);
        }

        buffers[n_buffers].length = buf.length;
        buffers[n_buffers].start = mmap(NULL, buf.length, PROT_READ | PROT_WRITE, MAP_SHARED, fd, buf.m.offset);

        if (buffers[n_buffers].start == MAP_FAILED) {
          free_buffers(buffers, n_buffers, method);
          return ERROR(mmap);
        }
      }
      break;
    case IO_METHOD_USERPTR:
      page_size = getpagesize();
      unsigned int buffer_size = (fmt.fmt.pix.sizeimage + page_size - 1) & ~(page_size - 1);

      if (!already_req) {
        CLEAR(req);

        req.count = 4;
        req.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
        req.memory = V4L2_MEMORY_USERPTR;

        if (xioctl(fd, VIDIOC_REQBUFS, &req) == -1) {
          if (errno == EINVAL) {
            return ERROR(no_userptr);
          }
          else {
            return ERROR(vidioc_reqbufs);
          }
        }
      }

      buffers = calloc(4, sizeof(*buffers));

      if (!buffers) {
        return ERROR(out_of_memory);
      }

      for (n_buffers = 0; n_buffers < 4; ++n_buffers) {
        buffers[n_buffers].length = buffer_size;
        buffers[n_buffers].start = memalign(page_size, buffer_size);

        if (!buffers[n_buffers].start) {
          free_buffers(buffers, n_buffers, method);
          return ERROR(out_of_memory);
        }
      }
      break;
  }

  if (!(obj = enif_alloc_resource(erv_video, sizeof(video_object_t)))) {
    close(fd);
    return enif_make_badarg(env);
  }

  obj->buffers = NULL;
  obj->n_buffers = 0;

  if (!(obj->env = enif_alloc_env())) {
    enif_release_resource(obj);
    close(fd);
    return enif_make_badarg(env);
  }

  obj->fd = fd;
  obj->width = width;
  obj->height = height;
  obj->method = method;
  obj->buffers = buffers;
  obj->n_buffers = n_buffers;
  obj->enabled = false;

  r = enif_make_tuple3(env, ATOM(ervideo),
      enif_make_ulong(env, (unsigned long) obj),
      enif_make_resource(env, obj));
  enif_release_resource(obj);

  return r;
}

#undef ERROR
#define ERROR(x)  _ERROR(x)

ERL_NIF_TERM
video_close(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM *tuple;
  int arity;
  video_object_t *obj;

  if (!IS_ERVIDEO(env, argv[0], tuple, arity, obj)) {
    return enif_make_badarg(env);
  }

  capture_off(obj);

  if (close(obj->fd)) {
    obj->fd = -1;
    return ERROR(already_closed);
  }

  obj->fd = -1;
  return enif_make_atom(env, "ok");
}

#undef ERROR
#define ERROR(x) _ERROR(x)

ERL_NIF_TERM
video_read(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM *tuple;
  int arity, res;
  video_object_t *obj;
  char inf[9], format[4];
  unsigned int timeout;
  unsigned char *frame;
  bool enabled;
  size_t len;

  if (!IS_ERVIDEO(env, argv[2], tuple, arity, obj)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_uint(env, argv[1], &timeout)) {
    if (enif_get_atom(env, argv[1], inf, 9, ERL_NIF_LATIN1) && !strcmp(inf, "infinity")) {
      timeout = 0;
    }
    else {
      return enif_make_badarg(env);
    }
  }

  if (!enif_get_atom(env, argv[0], format, 4, ERL_NIF_LATIN1)) {
    return enif_make_badarg(env);
  }

  enabled = obj->enabled;

  if (!enabled) {
    switch (capture_on(obj)) {
      case -1:
        return _ERROR(vidioc_qbuf);
      case -2:
        return _ERROR(vidioc_streamon);
    }
  }

  len = obj->width * obj->height * 2 * sizeof(unsigned char);

  for (;;) {
    fd_set fds;
    struct timeval tv;
    int r;

    FD_ZERO(&fds);
    FD_SET(obj->fd, &fds);

    tv.tv_sec = 1;
    tv.tv_usec = 0;

    r = select(obj->fd + 1, &fds, NULL, NULL, &tv);/*timeout == 0 ? NULL : &tv);*/

    if (r == -1) {
      if (errno == EINTR) {
        continue;
      }

      return ERROR(select);
    }

    if (r == 0) {
      continue;
      /*return ERROR(timeout);*/
    }

    if ((res = frame_read(obj, &frame)) > 0) {
      break;
    }

    switch (res) {
      case -1:
        return ERROR(closed);
      case -2:
        return ERROR(vidioc_dqbuf);
      case -3:
        return ERROR(empty_buffer);
      case -4:
        return ERROR(vidioc_qbuf);
      case -5:
        return ERROR(unknown_method);
    }
  }

  if (!enabled) {
    if (capture_off(obj) != 0) {
      return ERROR(vidioc_streamoff);
    }
  }

  if (!strcmp(format, "png")) {
    unsigned char *dst;
    int res = YUV422toPNG(obj->width, obj->height, frame, &dst, &len);

    switch (res) {
      case -1:
        return ERROR(out_of_memory);
      case -2:
        return ERROR(jmp_error);
      default:
        frame = dst;
    }

    ERL_NIF_TERM r;
    memcpy(enif_make_new_binary(env, len, &r), frame, len);

    return enif_make_tuple2(env, enif_make_atom(env, "png"), r);
  }
  else if (!strcmp(format, "yuyv")) {
    ERL_NIF_TERM r;
    memcpy(enif_make_new_binary(env, len, &r), frame, len);

    return enif_make_tuple4(env, enif_make_atom(env, "yuv"),
        r,
        enif_make_uint(env, obj->width),
        enif_make_uint(env, obj->height));
  }

  return ERROR(format);
}

static
int
ervideo_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
  (void) load_info;

  ErlNifResourceFlags tried;

  LOAD_ATOM(ervideo);

  erv_video = enif_open_resource_type(env, 0, "video",
      (ErlNifResourceDtor*) erv_video_dtor,
      ERL_NIF_RT_CREATE,
      &tried);
  *priv_data = 0;

  return 0;
}

ErlNifFunc erv_funcs[] = {
  { "open", 4, video_open },
  { "close", 1, video_close },
  { "read", 3, video_read }
};

ERL_NIF_INIT(ervideo, erv_funcs,
    ervideo_load, NULL, NULL, NULL);
