[
    Let(LetStatement { name: "counter", value: Function(FunctionLiteral { parameters: ["x"], body: BlockStatement { statements: [Expression(


If(IfExpression {
    condition: Infix(InfixExpression { left: Identifier("x"), operator: GT, right: Int(1) }),
    consequence: BlockStatement { statements: [Return(ReturnStatement { value: Boolean(true) })] },
    alternative: Some(
        BlockStatement {
            statements: [
                Expression(
                    Call(CallExpression {
                        function: Identifier("counter"),
                        arguments: [
                            Infix(InfixExpression { left: Identifier("x"), operator: Plus, right: Int(1) })
                        ]
                    }))
            ]
        })
    }))] } }) }),



    Expression(Call(CallExpression { function: Identifier("counter"), arguments: [Int(0)] }))
]