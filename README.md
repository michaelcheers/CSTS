# CSTS
A half-finished c# to Typescript transpiler

In future possibly extending to more target languages such as Java, c++, Python, etc.

# Ideas

## Generic Specialization

Take for example c++'s `vector<bool>`.
```csharp
public class List<T> : IList<T>
{
}

[GenericSpecialization]
public class BoolList : List<bool>
{
    // more efficient version of a List<bool>
}

```

The idea would be that any reference to `List<bool>` or `BoolList` would use `BoolList`.

This would require use of c++ style templates for transpiling to Typescript instead of the c# style of generics. Any types using a `List<T>` where `T` could be a `bool` would need to have separate implementations one with `BoolList` and one using the regular `List<T>`.
