package dk.itu.turbocharger.coq

object Charge {
  type class_j = String
  type var_j = String
  type ptr_j = (Int, class_j)
  type arrptr_j = Int
  type field_j = String
  type method_j = String

  sealed trait val_j extends CoqTerm
  object val_j {
    def vint(a : Int) : val_j =
      new ConstructorInvocation1("vint", ZTerm(a)) with val_j
    def vbool(a : Boolean) : val_j =
      new ConstructorInvocation1("vbool", BooleanTerm(a)) with val_j
    def vptr(a : ptr_j) : val_j =
      new ConstructorInvocation1("vptr", TupleTerm(a)) with val_j
    def varr(a : arrptr_j) : val_j =
      new ConstructorInvocation1("varr", NatTerm(a)) with val_j
    def nothing() : val_j = new ConstructorInvocation0("nothing") with val_j
  }

  sealed trait dexpr_j extends CoqTerm
  object dexpr_j {
    def E_val(a : val_j) : dexpr_j =
      new ConstructorInvocation1("E_val", a) with dexpr_j
    def E_var(a : var_j) : dexpr_j =
      new ConstructorInvocation1("E_var", StringTerm(a)) with dexpr_j
    def E_plus(a : dexpr_j, b : dexpr_j) : dexpr_j =
      new ConstructorInvocation2("E_plus", a, b) with dexpr_j
    def E_minus(a : dexpr_j, b : dexpr_j) : dexpr_j =
      new ConstructorInvocation2("E_minus", a, b) with dexpr_j
    def E_times(a : dexpr_j, b : dexpr_j) : dexpr_j =
      new ConstructorInvocation2("E_times", a, b) with dexpr_j
    def E_and(a : dexpr_j, b : dexpr_j) : dexpr_j =
      new ConstructorInvocation2("E_and", a, b) with dexpr_j
    def E_or(a : dexpr_j, b : dexpr_j) : dexpr_j =
      new ConstructorInvocation2("E_or", a, b) with dexpr_j
    def E_not(a : dexpr_j) : dexpr_j =
      new ConstructorInvocation1("E_not", a) with dexpr_j
    def E_lt(a : dexpr_j, b : dexpr_j) : dexpr_j =
      new ConstructorInvocation2("E_lt", a, b) with dexpr_j
    def E_eq(a : dexpr_j, b : dexpr_j) : dexpr_j =
      new ConstructorInvocation2("E_eq", a, b) with dexpr_j
  }

  sealed trait cmd_j extends CoqTerm with ConstructorInvocation
  object cmd_j {
    def cassign(a : var_j, b : dexpr_j) : cmd_j =
        new ConstructorInvocation2("cassign", StringTerm(a), b) with cmd_j
    def cskip() : cmd_j = new ConstructorInvocation0("cskip") with cmd_j
    def cseq(a : cmd_j, b : cmd_j) : cmd_j =
      new ConstructorInvocation2("cseq", a, b) with cmd_j
    def cif(a : dexpr_j, b : cmd_j, c : cmd_j) : cmd_j =
      new ConstructorInvocation3("cif", a, b, c) with cmd_j
    def cwhile(a : dexpr_j, b : cmd_j) : cmd_j =
      new ConstructorInvocation2("cwhile", a, b) with cmd_j
    def cread(a : var_j, b : var_j, c : field_j) : cmd_j =
      new ConstructorInvocation3("cread",
          StringTerm(a), StringTerm(b), StringTerm(c)) with cmd_j
    def cwrite(a : var_j, b : field_j, c : dexpr_j) : cmd_j =
      new ConstructorInvocation3("cwrite",
          StringTerm(a), StringTerm(b), c) with cmd_j
    def carrread(a : var_j, b : var_j, c : List[dexpr_j]) : cmd_j =
      new ConstructorInvocation3("carrread",
          StringTerm(a), StringTerm(b), ListTerm(c)) with cmd_j
    def carrwrite(a : var_j, b : List[dexpr_j], c : dexpr_j) : cmd_j =
      new ConstructorInvocation3("carrwrite",
          StringTerm(a), ListTerm(b), c) with cmd_j
    def carralloc(a : var_j, b : dexpr_j) : cmd_j =
      new ConstructorInvocation2("carralloc", StringTerm(a), b) with cmd_j
    def calloc(a : var_j, b : class_j) : cmd_j =
      new ConstructorInvocation2("calloc",
          StringTerm(a), StringTerm(b)) with cmd_j
    def cdcall(a : var_j, b : var_j, c : method_j, d : List[dexpr_j]) : cmd_j =
      new ConstructorInvocation4("cdcall",
          StringTerm(a), StringTerm(b), StringTerm(c), ListTerm(d)) with cmd_j
    def cscall(a : var_j,
        b : class_j, c : method_j, d : List[dexpr_j]) : cmd_j =
      new ConstructorInvocation4("cscall",
          StringTerm(a), StringTerm(b), StringTerm(c), ListTerm(d)) with cmd_j
    def cassert(a : dexpr_j) : cmd_j =
      new ConstructorInvocation1("cassert", a) with cmd_j
  }

  def cseqise(cs : List[cmd_j]) : cmd_j = cs match {
    case Nil =>
      cmd_j.cskip
    case (c : ConstructorInvocation0) :: a
        if c.constructor == "cskip" =>
      cseqise(a)
    case a :: (c : ConstructorInvocation0) :: Nil
        if c.constructor == "cskip" =>
      a
    case a :: Nil =>
      a
    case a :: b =>
      cmd_j.cseq(a, cseqise(b))
  }
}