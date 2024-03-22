#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include "caml/unixsupport.h"

CAMLprim value container_image_unix_lchown(value path, value uid, value gid)
{
  CAMLparam1(path);
  char * p; int ret;
  caml_unix_check_path(path, "lchown");
  p = caml_stat_strdup(String_val(path));
  caml_enter_blocking_section();
  ret = lchown(p, Int_val(uid), Int_val(gid));
  caml_leave_blocking_section();
  caml_stat_free(p);
  if (ret == -1) caml_uerror("lchown", path);
  CAMLreturn(Val_unit);
}
