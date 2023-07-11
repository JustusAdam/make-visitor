extern crate make_visitor;

make_visitor::make_visitor!(
    pub struct Expr(Box<ExprValue>);

    pub struct Ident(String);

    pub struct BigInt(u64);

    pub enum ExprValue {
        AddressOf(Expr),
        Array {
            elems: Vec<Expr>,
        },
        ArrayOf {
            elem: Expr,
        },
        Assign {
            left: Expr,
            right: Expr,
        },
        BinOp {
            op: BinaryOperator,
            lhs: Expr,
            rhs: Expr,
        },
        Dereference(Expr),
        DoubleConstant(f64),
        EmptyUnion,
        FloatConstant(f32),
        FunctionCall {
            function: Expr,
            arguments: Vec<Expr>,
        },
        If {
            c: Expr,
            t: Expr,
            e: Expr,
        },
        Index {
            array: Expr,
            index: Expr,
        },
        IntConstant(BigInt),
        Member {
            lhs: Expr,
            field: Ident,
        },
        PointerConstant(u64),
        SelfOp {
            op: SelfOperator,
            e: Expr,
        },
        StatementExpression {
            statements: Vec<Stmt>,
        },
        StringConstant {
            s: Ident,
        },
        Struct {
            values: Vec<Expr>,
        },
        Symbol {
            identifier: Ident,
        },
        Typecast(Expr),
        Union {
            value: Expr,
            field: Ident,
        },
        UnOp {
            op: UnaryOperator,
            e: Expr,
        },
        Vector {
            elems: Vec<Expr>,
        },
    }

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

    pub enum SelfOperator {
        Postdecrement,
        Postincrement,
        Predecrement,
        Preincrement,
    }

    pub enum UnaryOperator {
        Bitnot,
        BitReverse,
        Bswap,
        IsDynamicObject,
        IsFinite,
        Not,
        ObjectSize,
        PointerObject,
        PointerOffset,
        UnaryMinus,
    }

    pub struct Stmt {
        body: Box<StmtBody>,
    }

    pub enum StmtBody {
        AssignStmt {
            lhs: Expr,
            rhs: Expr,
        },
        AtomicBlock(Vec<Stmt>),
        Block(Vec<Stmt>),
        Break,
        Continue,
        Decl {
            lhs: Expr,
            value: Option<Expr>,
        },
        Deinit(Expr),
        Expression(Expr),
        For {
            init: Stmt,
            cond: Expr,
            update: Stmt,
            body: Stmt,
        },
        FunctionCall {
            lhs: Option<Expr>,
            function: Expr,
            arguments: Vec<Expr>,
        },
        Goto(Ident),
        Ifthenelse {
            i: Expr,
            t: Stmt,
            e: Option<Stmt>,
        },
        Label {
            label: Ident,
            body: Stmt,
        },
        Return(Option<Expr>),
        Skip,
        Switch {
            control: Expr,
            cases: Vec<SwitchCase>,
            default: Option<Stmt>,
        },
        While {
            cond: Expr,
            body: Stmt,
        },
    }

    pub struct SwitchCase {
        case: Expr,
        body: Stmt,
    }
);
