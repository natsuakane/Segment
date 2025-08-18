use lazy_static::lazy_static;
use regex::Regex;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::sync::Arc;
use std::sync::Mutex;

#[derive(Clone)]
enum Token {
    Number(String),
    StringLiteral(String),
    Identifier(String),
    Operator(String),
    EOF,
}
impl Token {
    pub fn print(&self) {
        match &self {
            Token::Number(num) => println!("num:{}", num),
            Token::StringLiteral(lit) => println!("literal:{}", lit),
            Token::Identifier(id) => println!("id:{}", id),
            Token::Operator(op) => println!("op:{}", op),
            Token::EOF => println!("EOF"),
        }
    }
    pub fn str(&self) -> String {
        match &self {
            Token::Number(num) => num.clone(),
            Token::StringLiteral(lit) => lit.clone(),
            Token::Identifier(id) => id.clone(),
            Token::Operator(op) => op.clone(),
            Token::EOF => "EOF".to_string(),
        }
    }
}

struct Lexer {
    code: String,
    iter: i32,
    que: VecDeque<Token>,
}
impl Lexer {
    fn new(program: String) -> Self {
        Lexer {
            code: program,
            iter: 0,
            que: VecDeque::new(),
        }
    }
    pub fn read(&mut self) -> Option<Token> {
        self.que.pop_front()
    }
    pub fn peek(&self) -> Option<Token> {
        self.que.front().map(|t| t.clone())
    }
    pub fn lex(&mut self) {
        let re =
            Regex::new(r#"(?P<num>\d+(\.\d+)?)|(?P<id>[a-zA-Z][a-zA-Z0-9_]*)|(?P<literal>"(?:\\.|[^"\\])*?")|(?P<op>[^\s\w"]+)"#)
                .unwrap();

        for cap in re.captures_iter(self.code.as_str()) {
            if let Some(m) = cap.name("num") {
                self.que.push_back(Token::Number(m.to_string()));
            } else if let Some(m) = cap.name("id") {
                self.que.push_back(Token::Identifier(m.to_string()));
            } else if let Some(m) = cap.name("literal") {
                self.que.push_back(Token::StringLiteral(m.to_string()));
            } else if let Some(m) = cap.name("op") {
                self.que.push_back(Token::Operator(m.to_string()));
            }
        }

        self.que.push_back(Token::EOF);
    }
}

#[derive(Clone, Debug)]
struct Type {
    name: String,
}

#[derive(Clone, Debug)]
enum AstNode {
    Empty,
    Float {
        val: f64,
    },
    Integer {
        val: i64,
    },
    String {
        val: String,
    },
    Identifier {
        name: String,
    },
    Operator {
        op: String,
        children: Vec<AstNode>,
    },
    Block {
        children: Vec<AstNode>,
    },
    List {
        children: Vec<AstNode>,
    },
    Let {
        name: String,
        is_mutable: bool,
        value_type: Type,
        value: Box<AstNode>,
    },
    If {
        condition: Box<AstNode>,
        then_branch: Box<AstNode>,
        else_branch: Box<AstNode>,
    },
    While {
        condition: Box<AstNode>,
        body: Box<AstNode>,
    },
    FunctionCall {
        func: Box<AstNode>,
        args: Vec<AstNode>,
    },
    Lambda {
        args: Vec<(String, Type)>,
        body: Box<AstNode>,
        return_type: Type,
    },
}
impl AstNode {
    fn str(&self) -> String {
        match &self {
            AstNode::Empty => String::from("Empty"),
            AstNode::Float { val } => format!("Float:{}", val),
            AstNode::Integer { val } => format!("Integer:{}", val),
            AstNode::String { val } => format!("String:{}", val),
            AstNode::Identifier { name } => format!("Identifier:{}", name),
            AstNode::Operator { op, children } => {
                let mut s = format!("Op:{}(", op);
                for child in children {
                    s.push_str(&child.str());
                    s.push(' ');
                }
                s.pop();
                s.push(')');
                s
            }
            AstNode::Block { children } => {
                let mut s = String::from("Block:{");
                for child in children {
                    s.push_str(&child.str());
                    s.push(' ');
                }
                s.pop();
                s.push('}');
                s
            }
            AstNode::List { children } => {
                let mut s = String::from("List:[");
                for child in children {
                    s.push_str(&child.str());
                    s.push(' ');
                }
                s.pop();
                s.push(']');
                s
            }
            AstNode::Let {
                name,
                is_mutable,
                value_type,
                value,
            } => {
                let mut s = String::from("Let:(");
                s.push_str(&name);
                s.push(' ');
                if *is_mutable {
                    s.push_str("mutable ");
                }
                s.push_str(&value_type.name);
                s.push(' ');
                s.push_str(&value.str());
                s.push(')');
                s
            }
            AstNode::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let mut s = String::from("If:(");
                s.push_str(&condition.str());
                s.push(' ');
                s.push_str(&then_branch.str());
                s.push_str(" else ");
                s.push_str(&else_branch.str());
                s.push(')');
                s
            }
            AstNode::While { condition, body } => {
                let mut s = String::from("While:(");
                s.push_str(&condition.str());
                s.push(' ');
                s.push_str(&body.str());
                s.push(')');
                s
            }
            AstNode::FunctionCall { func, args } => {
                let mut s = String::from("Call:");
                s.push_str(&func.str());
                s.push('(');
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        s.push(',');
                    }
                    s.push_str(&arg.str());
                }
                s.push(')');
                s
            }
            AstNode::Lambda {
                args,
                body,
                return_type,
            } => {
                let mut s = String::from("Lambda:");
                s.push('(');
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        s.push(',');
                    }
                    s.push_str(&arg.0);
                    s.push(':');
                    s.push_str(&arg.1.name);
                }
                s.push(')');
                s.push(':');
                s.push_str(&return_type.name);
                s.push(' ');
                s.push_str(&body.str());
                s.push(')');
                s
            }
        }
    }
}

struct Parser {
    lexer: Lexer,
    operators: HashMap<String, i32>,
}
impl Parser {
    fn new(lexer: Lexer) -> Self {
        let mut operators = HashMap::new();
        operators.insert("+".to_string(), 1);
        operators.insert("-".to_string(), 1);
        operators.insert("*".to_string(), 2);
        operators.insert("/".to_string(), 2);
        Self { lexer, operators }
    }

    fn token(&mut self, token: &str) -> Result<(), String> {
        match self.lexer.read().unwrap() {
            Token::Identifier(id) => {
                if id != token {
                    Err("Invalid token".to_string())
                } else {
                    Ok(())
                }
            }
            Token::Operator(op) => {
                if op != token {
                    Err("Invalid token".to_string())
                } else {
                    Ok(())
                }
            }
            _ => Err("Invalid token".to_string()),
        }
    }

    fn istoken(&self, token: &str) -> bool {
        match self.lexer.peek().unwrap() {
            Token::Identifier(id) => id == token,
            Token::Operator(op) => op == token,
            _ => false,
        }
    }

    fn isend(&self) -> bool {
        match self.lexer.peek().unwrap() {
            Token::EOF => true,
            _ => false,
        }
    }

    fn identifier(&mut self) -> Result<String, String> {
        match self.lexer.read().unwrap() {
            Token::Identifier(id) => Ok(id),
            _ => Err("Expected identifier".to_string()),
        }
    }

    fn get_type(&mut self) -> Result<Type, String> {
        let name = self.identifier()?;
        Ok(Type { name })
    }

    pub fn factor(&mut self) -> Result<AstNode, String> {
        match self.lexer.read().unwrap() {
            Token::Number(val) => {
                if val.to_string().contains(".") {
                    Ok(AstNode::Float {
                        val: val.parse().unwrap(),
                    })
                } else {
                    Ok(AstNode::Integer {
                        val: val.parse().unwrap(),
                    })
                }
            }
            Token::StringLiteral(val) => Ok(AstNode::String { val }),
            Token::Identifier(id) => Ok(AstNode::Identifier { name: id }),
            Token::Operator(op) => {
                if op == "(" {
                    let expr = self.lambda()?;
                    self.token(")")?;
                    Ok(expr)
                } else if op == "{" {
                    let mut statements = Vec::new();
                    while !self.istoken("}") {
                        statements.push(self.statement()?);
                    }
                    self.token("}")?;
                    println!("{}", statements.len());
                    Ok(AstNode::Block {
                        children: statements,
                    })
                } else {
                    Err("Invalid factor".to_string())
                }
            }
            _ => Err("Invalid factor".to_string()),
        }
    }

    pub fn funcall(&mut self) -> Result<AstNode, String> {
        let func = self.factor()?;
        if self.istoken("(") {
            self.token("(")?;
            let mut args = Vec::new();
            if !self.istoken(")") {
                args.push(self.expression(0)?);
                while self.istoken(",") {
                    self.token(",")?;
                    args.push(self.expression(0)?);
                }
            }
            self.token(")")?;
            Ok(AstNode::FunctionCall {
                func: Box::new(func),
                args,
            })
        } else {
            Ok(func)
        }
    }

    pub fn expression(&mut self, min_prec: i32) -> Result<AstNode, String> {
        let mut left = self.funcall()?;
        while let Some(Token::Operator(op)) = self.lexer.peek() {
            if !self.operators.contains_key(&op) {
                break;
            }
            let precedence = self.operators[&op];
            if precedence < min_prec {
                break;
            }
            self.lexer.read();
            let right = self.expression(precedence + 1)?;
            let ch: Vec<AstNode> = vec![left, right];
            left = AstNode::Operator { op, children: ch };
        }
        Ok(left)
    }

    pub fn lambda(&mut self) -> Result<AstNode, String> {
        if self.istoken("lambda") {
            self.token("lambda")?;
            self.token("(")?;
            let mut args: Vec<(String, Type)> = Vec::new();
            while !self.istoken(")") {
                let arg_name = self.identifier()?;
                self.token(":")?;
                let arg_type = self.get_type()?;
                args.push((arg_name, arg_type));
                if self.istoken(",") {
                    self.token(",")?;
                } else {
                    break;
                }
            }
            self.token(")")?;
            self.token(":")?;
            let return_type = self.get_type()?;
            self.token("=>")?;
            let body = self.expression(0)?;
            Ok(AstNode::Lambda {
                args,
                return_type,
                body: Box::new(body),
            })
        } else {
            self.expression(0)
        }
    }

    pub fn let_statement(&mut self) -> Result<AstNode, String> {
        if self.istoken("let") {
            self.token("let")?;
            let mut is_mutable = false;
            if self.istoken("mut") {
                self.token("mut")?;
                is_mutable = true;
            }
            let name = if let Some(t) = self.lexer.read() {
                match t {
                    Token::Identifier(id) => id,
                    _ => return Err("Expected identifier after 'let'".to_string()),
                }
            } else {
                return Err("Expected identifier after 'let'".to_string());
            };
            self.token(":")?;
            let value_type = match self.get_type() {
                Ok(t) => t,
                Err(_) => return Err("Expected identifier after ':'".to_string()),
            };
            self.token("=")?;
            Ok(AstNode::Let {
                name,
                is_mutable,
                value_type,
                value: Box::new(self.lambda()?),
            })
        } else {
            self.lambda()
        }
    }

    pub fn statement(&mut self) -> Result<AstNode, String> {
        if self.istoken("if") {
            self.token("if")?;
            let condition = Box::new(self.expression(0)?);
            let then_branch = Box::new(self.statement()?);
            if self.istoken("else") {
                self.token("else")?;
                let else_branch = Box::new(self.statement()?);
                Ok(AstNode::If {
                    condition,
                    then_branch,
                    else_branch,
                })
            } else {
                Ok(AstNode::If {
                    condition,
                    then_branch,
                    else_branch: Box::new(AstNode::Empty),
                })
            }
        } else if self.istoken("while") {
            self.token("while")?;
            let condition = Box::new(self.expression(0)?);
            let body = Box::new(self.statement()?);
            Ok(AstNode::While { condition, body })
        } else {
            self.let_statement()
        }
    }
}

#[derive(Clone)]
enum ValueContent {
    Bit(bool),
    Array(Vec<Value>),
    Object(HashMap<String, Value>),
    Function(Arc<dyn Fn(Vec<Value>) -> Value + Send + Sync + 'static>),
}
#[derive(Clone)]
struct Value {
    content: ValueContent,
    value_type: String,
}

struct Environment {
    variables: HashMap<String, (Value, bool)>,
}
impl Environment {
    fn new() -> Self {
        Environment {
            variables: HashMap::new(),
        }
    }

    fn get(&self, name: &str) -> Result<&Value, String> {
        self.variables
            .get(name)
            .map(|(value, _)| value)
            .ok_or_else(|| format!("Variable '{}' not found", name))
    }

    fn set(&mut self, name: String, value: Value) -> Result<(), String> {
        if let Some((_, is_mut)) = self.variables.get(&name) {
            if *is_mut {
                self.variables.insert(name, (value, *is_mut));
                Ok(())
            } else {
                Err(format!("Variable '{}' is not mutable", name))
            }
        } else {
            Err(format!("Variable '{}' not found", name))
        }
    }

    fn assign(&mut self, name: String, value: Value, is_mut: bool) -> Result<(), String> {
        if let Some((_, _)) = self.variables.get(&name) {
            Err(format!("Variable '{}' is already defined", name))
        } else {
            self.variables.insert(name, (value, is_mut));
            Ok(())
        }
    }
}
struct Environments {
    stack: VecDeque<Environment>,
}
impl Environments {
    fn new() -> Self {
        let mut vec = VecDeque::new();
        vec.push_back(Environment::new());
        Environments { stack: vec }
    }

    fn push(&mut self, env: Environment) {
        self.stack.push_back(env);
    }

    fn pop(&mut self) -> Option<Environment> {
        self.stack.pop_back()
    }

    fn get(&self, name: &str) -> Result<&Value, String> {
        for env in self.stack.iter().rev() {
            if let Ok(value) = env.get(name) {
                return Ok(value);
            }
        }
        Err(format!("Variable '{}' not found", name))
    }

    fn set(&mut self, name: String, value: Value) -> Result<(), String> {
        if let Some(env) = self.stack.back_mut() {
            env.set(name, value)
        } else {
            Err("No environment to set variable".to_string())
        }
    }

    fn assign(&mut self, name: String, value: Value, is_mut: bool) -> Result<(), String> {
        if let Some(env) = self.stack.back_mut() {
            env.assign(name, value, is_mut)
        } else {
            Err("No environment to assign variable".to_string())
        }
    }
}

lazy_static! {
    static ref ENVIRONMENTS: Mutex<Environments> = Mutex::new(Environments::new());
}

impl AstNode {
    fn eval(&self) -> Result<Value, String> {
        match self {
            AstNode::Empty => Err("The AstNode is Empty".to_string()),
            AstNode::Float { val } => {
                let bit_u64 = val.to_bits();
                let mut bits: Vec<Value> = vec![
                    Value {
                        content: ValueContent::Bit(false),
                        value_type: "Bit".to_string(),
                    };
                    64
                ];
                for i in 0..64 {
                    bits[i].content =
                        ValueContent::Bit(if bit_u64 >> i & 1 == 1 { true } else { false });
                }
                Ok(Value {
                    content: ValueContent::Array(bits),
                    value_type: "Float".to_string(),
                })
            }
            AstNode::Integer { val } => {
                let bit_u64 = *val as u64;
                let mut bits: Vec<Value> = vec![
                    Value {
                        content: ValueContent::Bit(false),
                        value_type: "Bit".to_string(),
                    };
                    64
                ];
                for i in 0..64 {
                    bits[i].content =
                        ValueContent::Bit(if bit_u64 >> i & 1 == 1 { true } else { false });
                }
                Ok(Value {
                    content: ValueContent::Array(bits),
                    value_type: "Integer".to_string(),
                })
            }
            AstNode::String { val } => {
                let bytes = val.as_bytes();
                let mut bits: Vec<Value> = vec![
                    Value {
                        content: ValueContent::Bit(false),
                        value_type: "Bit".to_string(),
                    };
                    8 * bytes.len()
                ];
                for i in 0..bytes.len() {
                    for j in 0..8 {
                        bits[i * 8 + j].content =
                            ValueContent::Bit(if bytes[i] >> j & 1 == 1 { true } else { false });
                    }
                }
                Ok(Value {
                    content: ValueContent::Array(bits),
                    value_type: "String".to_string(),
                })
            }
            _ => Err("Not defined".to_string()),
        }
    }
}

fn main() {
    let mut lexer = Lexer::new("1+2".to_string());
    lexer.lex();
    let mut parser = Parser::new(lexer);
    let node = parser.statement().unwrap();
    println!("{}", node.str());
    node.eval();
}
