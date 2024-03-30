//options log=false, optimize = true
options rtti = true

require expr_print
require daslib/ast_boost

struct SaySomethingParams
    a, b : int

def function1()
    var params <- new [[SaySomethingParams() a = 1, b = 2]]
    return params

def function2()
    var params <- new [[SaySomethingParams() a = 1, b = 2]]
    return params

typedef
    Integer = int
    IorF = variant<i:int;f:float>

enum FooBar
    Foo
    Bar = 13


[comment(x=5,msg="haha")]
struct Foo
    [[do_not_delete,temp=13]] a : int = 11
    b : array<Foo?>

def Foo(x:int)
    return <- [[Foo a=x]]

def Foo(x,y:int)
    return <- [[Foo a=x+y]]

var
    add_extra = 13

[sideeffects]
def add(a,b:int)
    print("a={a} b={b}")
    return a + b + add_extra

[sideeffects,export]
def allExpr(arg:int)
    // ExprStringBuilder
    var sb = "arg = {arg}\n"
    sb = ""
    // ExprNamedCall
    add([a=1,b=2])
    // ExprNew
    var pFoo1 = new Foo
    var pFoo2 = new Foo(1)
    var pFoo3 = new Foo(1,2)
    // ExprCall
    add(1,2)
    // ExprNullCoalescing
    var exprAt <- pFoo1 ?? [[Foo a=1]]
    exprAt <- [[Foo a=2]]
    // ExprAt
    pFoo1.b[0] = pFoo2
    // ExprSafeAt
    var exprSafeAt = pFoo1.b?[0] ?? pFoo2
    exprSafeAt = pFoo2
    // ExprIs
    if exprAt is type<int>
        print("nada\n")
    // ExprOp2
    var aa = 1
    aa = 3
    var bb = 2
    bb = 4
    var cc = aa + bb
    // ExprOp3
    cc = aa==bb ? aa : bb
    // ExprWith
    with exprAt
        aa = 3
    // ExprWhile
    while aa!=100
        aa++
    // ExprTryRecover
    try
        aa = 2
    recover
        aa = 3
    // ExprIfThenElse
    if aa==2
        aa = 2
    if aa==2
        aa = 2
    else
        aa = 3
    if aa==2
        aa = 2
    elif aa==3
        aa = 3
    else
        aa = 4
    // ExprMakeVariant
    var mkv = [[IorF i=5]]
    mkv = [[IorF f=1.0]]
    // ExprMakeStruct
    var mks <- [[Foo() a=5, b<-[{Foo? [1] null}]; a=6]]
    mks[1] <- [[Foo a=5]]
    // ExprMakeArray
    var mka = [[int 1;2;3;4]]
    mka = [[int 5;6;7;8]]
    // ExprMakeTuple
    var mkt = [[auto 1,2.0,"three"]]
    mkt = [[auto 2,3.0,"four"]]
    // ExprArrayCompehension
    var acomp <- [{for x in range(3); x*x; where x!=1}]
    var gcomp <- [[for x in range(3); x*x; where x!=1]]
    for ta,tg in acomp,gcomp
        verify(ta==tg)
    // ExprTypeInfo
    var tinfo = typeinfo(rtti_typeinfo type<int>).basicType
    print("{tinfo}\n")
    // ExprPtr2Ref
    print("{*pFoo1}")
    // ExprLabel, ExprGoto
    goto label 1
    print("nop")
    label 1:
    // ExprRef2Ptr
    unsafe
        let aacomp = addr(acomp)
        print("{aacomp}")
    // ExprAddr
    let eaddr = @@add
    print("{eaddr}")
    // ExprAssert, ExprStaticAssert
    assert(aa==4,"assert here")
    verify(aa==4,"verify here")
    static_assert(true,"static assert here")
    concept_assert(true,"concept assert here")
    // ExprDebug
    debug(aa)
    // ExprInvoke
    let atadd = @@add
    invoke(atadd,1,2)
    // ExprErase
    var tab <- {{ 1 => "one"; 2 => "two" }}
    erase(tab,1)
    // ExprFind
    find(tab, 2) <| $(argX)
        print("{argX}")
    // ExprKeyExists
    assert(key_exists(tab,2))
    // ExprAscend
    let easc = new [[auto 1;2;3;4]]
    print("{easc}")
    // ExprCast
    unsafe
        let aaf = reinterpret<float> aa
        print("{aaf}")
    // ExprDelete
    unsafe
        delete pFoo1
    // ExprSwizzle
    var fsw = float4(1,2,3,4)
    fsw = fsw.yyzx
    // ExprOp1
    var eop1 = 1
    eop1 ++
    -- eop1
    // ExprMemZero
    memzero(eop1)
    // ExprConstString
    var cstr = "hello\nworld\n"
    cstr += "\{..\}"
    // ExprConstEnumeration
    var enc = Type none
    enc = Type tInt
    // ExprConstBitfield
    var bfc = ExprPrintFlags topLevel
    bfc = ExprPrintFlags argLevel

def FString(c)
    return c
def call_ufunction_on(obj, funcName, params)
    pass

//[unreal_call_params]
//struct sayHelloWorldParams
//    a: int

[export]
def main()
    print_expression(@@function1)
    print_expression(@@function2)
    print_expression(@@allExpr)