
AC_DEFUN([AX_PGI_VERSION], [
    PGI_VERSION=""
    ax_pgi_version_option=yes
  AS_IF([test "x$FC" = "xyes"],[
    AS_IF([test "x$ax_pgi_version_option" != "xno"],[
      AC_CACHE_CHECK([pgi version],[ax_cv_pgi_version],[
        ax_cv_pgi_version="`$FC -V | grep pgf | awk '{print $2}'`"
        AS_IF([test "x$ax_cv_pgi_version" = "x"],[
          ax_cv_pgi_version=""
        ])
      ])
      PGI_VERSION=$ax_cv_pgi_version
    ])
  ])
  AC_SUBST([PGI_VERSION])
])
