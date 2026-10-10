using System;
using System.Collections.Generic;

public record Person(string Name, int Age);
public sealed record class Employee(string Name, int Age, int Id) : Person(Name, Age);
public readonly record struct Point(double X, double Y);
public record Pair<T>(T Left, T Right) where T : class;
public record Empty;

public record PersonWithBody(string Name)
{
    public string Nickname { get; init; }
    public string Greeting => "Hello " + Name;
    public record Nested(int Value);
}

public class FollowingClass
{
    public string Description = "record Fake(int Value);";
    // record CommentedOut(string Name);
    public void Check()
    {
        var record = new Person("Ada", 36);
        Console.WriteLine(record.Name);
    }
}
