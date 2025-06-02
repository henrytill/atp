{-# LANGUAGE CPP #-}

module Intro.SemanticsTests.Data where

import Intro.Syntax (Expression (..))

simplify :: [(Expression, String)]
simplify =
#define VAR(x, input) (Var x, input) :
#define CONST(x, input) (Const x, input) :
#include "intro_simplify.def"
#undef VAR
#undef CONST
  []

simplifyPartial :: [(String, String)]
simplifyPartial =
#define PARTIAL(output, input) (output, input) :
#include "intro_simplify_partial.def"
#undef PARTIAL
  []

simplifyWithCount :: [(Expression, Int, String)]
simplifyWithCount =
#define COUNTED_VAR(x, n, input) (Var x, n, input) :
#define COUNTED_CONST(x, n, input) (Const x, n, input) :
#include "intro_simplify_with_count.def"
#undef COUNTED_VAR
#undef COUNTED_CONST
  []
