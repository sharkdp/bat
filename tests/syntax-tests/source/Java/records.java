import java.util.List;

sealed interface Shape permits Circle, Polygon {}
record Circle(double radius) implements Shape {}
non-sealed class Polygon implements Shape {}
public record Pair<T>(T first, T second) {}
record Annotated(@Deprecated String name, int[] values, String... tags) {}

sealed class Parent permits Child, OtherChild {}
final class Child extends Parent {}
non-sealed class OtherChild extends Parent {}

class Container {
    public record Nested(String name) {
        public String greeting() { return "Hello " + name; }
    }
    void test() {
        record Local(int value) {}
        int record = 1;
        int sealed = 2;
        String text = "record Fake(int value) {} sealed class Fake {}";
        // record CommentedOut(String value) {}
        System.out.println(record + sealed);
    }
}

class FollowingClass {}
