extern crate cv;

type InternedString = &'static str;
type Location = ();
type Parameter = ();
type Type = ();
type BigInt = ();

cv::make_visitor!(
    #[derive(Clone, Debug)]
    pub struct Expr {
        value: Box<ExprValue>,
        typ: Type,
        location: Location,
        size_of_annotation: Option<Type>,
    }

    /// The different kinds of values an expression can have.
    /// The names are chosen to map directly onto the IrepID used by CBMC.
    /// Each expression is described by reference to the corresponding C code that would generate it.
    /// When an expression makes most sense in a broader statement context,
    /// the characters >>> e <<< are used to mark the part described by the enum.
    #[derive(Debug, Clone)]
    pub enum ExprValue {
        /// `&self`
        AddressOf(Expr),
        /// `typ x[] = >>> {elems0, elems1 ...} <<<`
        Array {
            elems: Vec<Expr>,
        },
        /// `typ x[width] = >>> {elem} <<<`
        ArrayOf {
            elem: Expr,
        },
        /// `left = right`
        Assign {
            left: Expr,
            right: Expr,
        },
        /// `lhs op rhs`.  E.g. `lhs + rhs` if `op == BinaryOperator::Plus`
        BinOp {
            op: BinaryOperator,
            lhs: Expr,
            rhs: Expr,
        },
        /// `(__CPROVER_bool) >>> true/false <<<`. True/False as a single bit boolean.
        BoolConstant(bool),
        /// Reinterpret bytes of e as type self.typ
        ByteExtract {
            e: Expr,
            offset: u64,
        },
        /// `(bool) 1`. True false as an 8 bit c_boolean.
        CBoolConstant(bool),
        /// `*self`
        Dereference(Expr),
        /// `1.0`
        DoubleConstant(f64),
        // {}
        EmptyUnion,
        /// `1.0f`
        FloatConstant(f32),
        /// `function(arguments)`
        FunctionCall {
            function: Expr,
            arguments: Vec<Expr>,
        },
        /// `c ? t : e`
        If {
            c: Expr,
            t: Expr,
            e: Expr,
        },
        /// `array[index]`
        Index {
            array: Expr,
            index: Expr,
        },
        /// `123`
        IntConstant(BigInt),
        /// `lhs.field`
        Member {
            lhs: Expr,
            field: InternedString,
        },
        /// `__nondet()`
        Nondet,
        /// `NULL`
        PointerConstant(u64),
        // `op++` etc
        SelfOp {
            op: SelfOperator,
            e: Expr,
        },
        /// <https://gcc.gnu.org/onlinedocs/gcc/Statement-Exprs.html>
        /// e.g. `({ int y = foo (); int z; if (y > 0) z = y; else z = - y; z; })`
        /// `({ op1; op2; ...})`
        StatementExpression {
            statements: Vec<Stmt>,
        },
        /// A raw string constant. Note that you normally actually want a pointer to the first element.
        /// `"s"`
        StringConstant {
            s: InternedString,
        },
        /// Struct initializer
        /// `struct foo the_foo = >>> {field1, field2, ... } <<<`
        Struct {
            values: Vec<Expr>,
        },
        /// `self`
        Symbol {
            identifier: InternedString,
        },
        /// `(typ) self`. Target type is in the outer `Expr` struct.
        Typecast(Expr),
        /// Union initializer
        /// `union foo the_foo = >>> {.field = value } <<<`
        Union {
            value: Expr,
            field: InternedString,
        },
        // `op self` eg `! self` if `op == UnaryOperator::Not`
        UnOp {
            op: UnaryOperator,
            e: Expr,
        },
        /// `vec_typ x = >>> {elems0, elems1 ...} <<<`
        Vector {
            elems: Vec<Expr>,
        },
        Quantify {
            quantifier: Quantifier,
            parameter: Parameter,
            body: Expr,
        },
        Old(Expr),
        /// A special expression type used in `assigns` contract clauses. See
        /// <https://diffblue.github.io/cbmc/contracts-assigns.html>
        ConditionalTargetGroup {
            condition: Box<Expr>,
            targets: Vec<MemoryTarget>,
        },
        MemoryTarget(MemoryTarget),
    }

    #[derive(Clone, Debug, Copy, Eq, PartialEq, PartialOrd)]
    pub enum Quantifier {
        Forall,
        Exists,
    }

    /// A target of an assigns clause. Used in [`ExprValue::ConditonalTargetGroup`]. See
    /// also <https://diffblue.github.io/cbmc/contracts-assigns.html>
    #[derive(Debug, Clone)]
    pub enum MemoryTarget {
        /// lvalue-expr
        Lvalue(Expr),
        // | __CPROVER_typed_target(lvalue-expr)
        ObjectWhole(Expr),
        // | __CPROVER_object_from(ptr-expr)
        // | __CPROVER_object_upto(ptr-expr, uint-expr)
    }

    /// Binary operators. The names are the same as in the Irep representation.
    #[derive(Debug, Clone, Copy)]
    pub enum BinaryOperator {
        And,
        Ashr,
        Bitand,
        Bitor,
        Bitnand,
        Bitxor,
        Div,
        Equal,
        Ge,
        Gt,
        IeeeFloatEqual,
        IeeeFloatNotequal,
        Implies,
        Le,
        Lshr,
        Lt,
        Minus,
        Mod,
        Mult,
        Notequal,
        Or,
        OverflowMinus,
        OverflowMult,
        OverflowPlus,
        OverflowResultMinus,
        OverflowResultMult,
        OverflowResultPlus,
        Plus,
        ROk,
        Rol,
        Ror,
        Shl,
        VectorEqual,
        VectorNotequal,
        VectorGe,
        VectorGt,
        VectorLe,
        VectorLt,
        Xor,
    }

    // Unary operators with side-effects
    #[derive(Debug, Clone, Copy)]
    pub enum SelfOperator {
        /// `self--`
        Postdecrement,
        /// `self++`
        Postincrement,
        /// `--self`
        Predecrement,
        /// `++self`
        Preincrement,
    }

    #[derive(Debug, Clone, Copy)]
    pub enum UnaryOperator {
        /// `~self`
        Bitnot,
        /// `__builtin_bitreverse<n>(self)`
        BitReverse,
        /// `__builtin_bswap<n>(self)`
        Bswap,
        /// `__CPROVER_DYNAMIC_OBJECT(self)`
        IsDynamicObject,
        /// `isfinite(self)`
        IsFinite,
        /// `!self`
        Not,
        /// `__CPROVER_OBJECT_SIZE(self)`
        ObjectSize,
        /// `__CPROVER_POINTER_OBJECT(self)`
        PointerObject,
        /// `__CPROVER_POINTER_OFFSET(self)`
        PointerOffset,
        /// `__builtin_popcount(self)`
        Popcount,
        /// `__builtin_cttz(self)`
        CountTrailingZeros { allow_zero: bool },
        /// `__builtin_ctlz(self)`
        CountLeadingZeros { allow_zero: bool },
        /// `-self`
        UnaryMinus,
    }

    /// The return type for `__CPROVER_overflow_op` operations
    pub struct ArithmeticOverflowResult {
        /// If overflow did not occur, the result of the operation. Otherwise undefined.
        pub result: Expr,
        /// Boolean: true if overflow occured, false otherwise.
        pub overflowed: Expr,
    }

    #[derive(Clone, Debug)]
    pub struct Stmt {
        body: Box<StmtBody>,
        location: Location,
    }
    /// The different kinds of bodies a statement can have.
    /// The names are chosen to map directly onto the IrepID used by CBMC.
    /// Each statement is described by reference to the corresponding C code that would generate it.
    #[derive(Debug, Clone)]
    pub enum StmtBody {
        /// `lhs = rhs;`
        AssignStmt { lhs: Expr, rhs: Expr },
        /// `assert(cond)`
        Assert {
            cond: Expr,
            property_class: InternedString,
            msg: InternedString,
        },
        /// `__CPROVER_assume(cond);`
        Assume { cond: Expr },
        /// { ATOMIC_BEGIN stmt1; stmt2; ... ATOMIC_END }
        AtomicBlock(Vec<Stmt>),
        /// `{ stmt1; stmt2; ... }`
        Block(Vec<Stmt>),
        /// `break;`
        Break,
        /// `continue;`
        Continue,
        /// `lhs.typ lhs = value;` or `lhs.typ lhs;`
        Decl {
            lhs: Expr, // SymbolExpr
            value: Option<Expr>,
        },
        /// Marks the target place as uninitialized.
        Deinit(Expr),
        /// `e;`
        Expression(Expr),
        // `for (init; cond; update) {body}`
        For {
            init: Stmt,
            cond: Expr,
            update: Stmt,
            body: Stmt,
        },
        /// `lhs = function(arguments);` or `function(arguments);`
        FunctionCall {
            lhs: Option<Expr>,
            function: Expr,
            arguments: Vec<Expr>,
        },
        /// `goto dest;`
        Goto(InternedString),
        /// `if (i) { t } else { e }`
        Ifthenelse { i: Expr, t: Stmt, e: Option<Stmt> },
        /// `label: body;`
        Label { label: InternedString, body: Stmt },
        /// `return e;` or `return;`
        Return(Option<Expr>),
        /// `;`
        Skip,
        /// `switch (control) { case1.case: cast1.body; case2.case: case2.body; ... }`
        Switch {
            control: Expr,
            cases: Vec<SwitchCase>,
            default: Option<Stmt>,
        },
        /// `while (cond) { body }`
        While { cond: Expr, body: Stmt },
    }

    #[derive(Debug, Clone)]
    pub struct SwitchCase {
        case: Expr,
        body: Stmt,
    }
);
