AC_INIT(lurch, 0.0.1)
m4_include([m4/ocaml.m4])

AC_ARG_VAR(OCAMLOPTFLAGS, [Additional flags to pass to the ocaml compiler])
AC_ARG_VAR(OCAMLCFLAGS, [Additional flags to pass to the ocaml bytecode compiler])

AC_PROG_OCAML
AS_IF(
  [test "$OCAMLC" = no],
  AC_MSG_ERROR([You must install the OCaml compiler.]))

AC_PROG_FINDLIB
AS_IF(
  [test "$OCAMLFIND" = no],
  AC_MSG_ERROR([You must install OCaml findlib (the ocamlfind command).]))

AC_CHECK_OCAML_PKG(batteries)
AC_CHECK_OCAML_PKG(cgi)
AC_CHECK_OCAML_PKG(cmdliner)
AC_CHECK_OCAML_PKG(js_of_ocaml)
AC_CHECK_OCAML_PKG(js_of_ocaml-ppx)
AC_CHECK_OCAML_PKG(js_of_ocaml-ppx_deriving_json)
AC_CHECK_OCAML_PKG(ocaml-vdom)
AC_CHECK_OCAML_PKG(postgresql)
AC_CHECK_OCAML_PKG(syslog)

AC_PATH_PROG(CONVERT, convert)
if test -z "$CONVERT"; then
  AC_MSG_ERROR([Please install imagemagick (for convert).])
fi
AC_SUBST([CONVERT])

AC_CONFIG_FILES([
  Makefile
  opam
  src/LurchVersion.ml
])
AC_OUTPUT
