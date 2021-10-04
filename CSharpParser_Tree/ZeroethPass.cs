using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace CSharpParser_Tree
{
    using static Basics.ExceptionThrower;

    public class ZeroethPass : CSharpSyntaxRewriter
    {
        public SemanticModel model;
        public ZeroethPass (SemanticModel model)
        {
            this.model = model;
        }
        public static string GenerateVarName () => "_" + Guid.NewGuid().ToString("N").ToUpper();
        public static InvocationExpressionSyntax GenerateBlockLambdaInvocation(ITypeSymbol type, params StatementSyntax[] statements) =>
            GenerateBlockLambdaInvocation(type, (IEnumerable<StatementSyntax>)statements);
        public static InvocationExpressionSyntax GenerateBlockLambdaInvocation(ITypeSymbol type, IEnumerable<StatementSyntax> statements) =>
            SyntaxFactory.InvocationExpression(
                SyntaxFactory.ParenthesizedExpression(
                    SyntaxFactory.CastExpression(
                        SyntaxFactory.ParseName("global::System.Func<" + type.Name + ">"),
                        SyntaxFactory.ParenthesizedExpression(
                            SyntaxFactory.ParenthesizedLambdaExpression(
                                SyntaxFactory.ParameterList(),
                                SyntaxFactory.Block(statements)
                            )
                        )
                    )
                )
            );
        public override SyntaxNode? VisitAssignmentExpression(AssignmentExpressionSyntax node)
        {
            if (node.Kind() is SyntaxKind.SimpleAssignmentExpression or SyntaxKind.CoalesceAssignmentExpression)
                return base.VisitAssignmentExpression(node);
            ExpressionSyntax GenerateAssignment(ExpressionSyntax operand) =>
                SyntaxFactory.AssignmentExpression(
                    SyntaxKind.SimpleAssignmentExpression,
                    operand,
                    SyntaxFactory.BinaryExpression(
                        node.Kind() switch
                        {
                            SyntaxKind.AddAssignmentExpression => SyntaxKind.AddExpression,
                            SyntaxKind.AndAssignmentExpression => SyntaxKind.LogicalAndExpression,
                            SyntaxKind.DivideAssignmentExpression => SyntaxKind.DivideExpression,
                            SyntaxKind.ExclusiveOrAssignmentExpression => SyntaxKind.ExclusiveOrExpression,
                            SyntaxKind.LeftShiftAssignmentExpression => SyntaxKind.LeftShiftExpression,
                            SyntaxKind.ModuloAssignmentExpression => SyntaxKind.ModuloExpression,
                            SyntaxKind.MultiplyAssignmentExpression => SyntaxKind.MultiplyExpression,
                            SyntaxKind.OrAssignmentExpression => SyntaxKind.LogicalOrExpression,
                            SyntaxKind.RightShiftAssignmentExpression => SyntaxKind.RightShiftExpression,
                            SyntaxKind.SubtractAssignmentExpression => SyntaxKind.SubtractExpression,
                        },
                        operand,
                        node.Right
                    )
                );
            switch (node.Left)
            {
                case IdentifierNameSyntax op:
                    return GenerateAssignment(op);
                case MemberAccessExpressionSyntax maes:
                    string leftVarName = GenerateVarName();
                    return GenerateBlockLambdaInvocation(model.GetTypeInfo(node.Left).Type!,
                        SyntaxFactory.LocalDeclarationStatement(
                            SyntaxFactory.VariableDeclaration(
                                SyntaxFactory.IdentifierName("var"),
                                SyntaxFactory.SeparatedList(new[]{
                                    SyntaxFactory.VariableDeclarator(
                                        identifier: SyntaxFactory.Identifier(leftVarName)
                                    ).WithInitializer(
                                        SyntaxFactory.EqualsValueClause(maes.Expression)
                                    )
                                })
                            )
                        ),
                        SyntaxFactory.ReturnStatement(
                            GenerateAssignment(
                                maes.WithExpression(
                                    SyntaxFactory.IdentifierName(leftVarName)
                                )
                            )
                        )
                    );
                default:
                    throw E;
            }
        }
        public override SyntaxNode? VisitPrefixUnaryExpression(PrefixUnaryExpressionSyntax node)
        {
            ExpressionSyntax GenerateAssignment(ExpressionSyntax operand) =>
                SyntaxFactory.AssignmentExpression(
                    SyntaxKind.SimpleAssignmentExpression,
                    operand,
                    SyntaxFactory.BinaryExpression(
                        node.Kind() switch
                        {
                            SyntaxKind.PreIncrementExpression => SyntaxKind.AddExpression,
                            SyntaxKind.PreDecrementExpression => SyntaxKind.SubtractExpression
                        },
                        operand,
                        SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(1))
                    )
                );
            switch (node.Operand)
            {
                case IdentifierNameSyntax op:
                    return GenerateAssignment(op);
                case MemberAccessExpressionSyntax maes:
                    string leftVarName = GenerateVarName();
                    return GenerateBlockLambdaInvocation(model.GetTypeInfo(node.Operand).Type!,
                        SyntaxFactory.LocalDeclarationStatement(
                            SyntaxFactory.VariableDeclaration(
                                SyntaxFactory.IdentifierName("var"),
                                SyntaxFactory.SeparatedList(new[]{
                                        SyntaxFactory.VariableDeclarator(
                                            identifier: SyntaxFactory.Identifier(leftVarName)
                                        ).WithInitializer(
                                            SyntaxFactory.EqualsValueClause(maes.Expression)
                                        )
                                })
                            )
                        ),
                        SyntaxFactory.ReturnStatement(
                            GenerateAssignment(
                                maes.WithExpression(
                                    SyntaxFactory.IdentifierName(leftVarName)
                                )
                            )
                        )
                    );
                default:
                    throw E;
            }
        }
        public override SyntaxNode? VisitPostfixUnaryExpression(PostfixUnaryExpressionSyntax node)
        {
            IEnumerable<StatementSyntax> Statements()
            {
                ExpressionSyntax operand;
                switch (node.Operand)
                {
                    case IdentifierNameSyntax op:
                        operand = op;
                        break;
                    case MemberAccessExpressionSyntax maes:
                        string leftVarName = GenerateVarName();
                        operand = maes.WithExpression(SyntaxFactory.IdentifierName(leftVarName));
                        yield return SyntaxFactory.LocalDeclarationStatement(
                            SyntaxFactory.VariableDeclaration(
                                SyntaxFactory.IdentifierName("var"),
                                SyntaxFactory.SeparatedList(new[]{
                                            SyntaxFactory.VariableDeclarator(
                                                identifier: SyntaxFactory.Identifier(leftVarName)
                                            ).WithInitializer(
                                                SyntaxFactory.EqualsValueClause(maes.Expression)
                                            )
                                })
                            )
                        );
                        break;
                    default:
                        throw E;
                }
                string varName = GenerateVarName();
                yield return SyntaxFactory.LocalDeclarationStatement(
                    SyntaxFactory.VariableDeclaration(
                        SyntaxFactory.IdentifierName("var"),
                        SyntaxFactory.SeparatedList(new[] {
                                SyntaxFactory.VariableDeclarator(
                                    identifier: SyntaxFactory.Identifier(varName)
                                ).WithInitializer(
                                    SyntaxFactory.EqualsValueClause(operand)
                                )
                        })
                    )
                );
                yield return SyntaxFactory.ExpressionStatement(
                    SyntaxFactory.AssignmentExpression(
                        SyntaxKind.SimpleAssignmentExpression,
                        operand,
                        SyntaxFactory.BinaryExpression(
                            node.Kind() switch
                            {
                                SyntaxKind.PostIncrementExpression => SyntaxKind.AddExpression,
                                SyntaxKind.PostDecrementExpression => SyntaxKind.SubtractExpression
                            },
                            SyntaxFactory.IdentifierName(varName),
                            SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(1))
                        )
                    )
                );
                yield return SyntaxFactory.ReturnStatement(SyntaxFactory.IdentifierName(varName));
            }
            return GenerateBlockLambdaInvocation(model.GetTypeInfo(node.Operand).Type!, Statements());
        }
    }

    public static partial class Program
    {
        public static partial CSharpSyntaxTree ZeroethPass(CSharpSyntaxTree tree) =>
            (CSharpSyntaxTree)tree.WithRootAndOptions(
                new ZeroethPass(
                    model: CSharpCompilation.Create("HelloWorld").AddSyntaxTrees(tree).GetSemanticModel(tree)
                ).Visit(tree.GetRoot()),
                tree.Options
            );
    }
}
