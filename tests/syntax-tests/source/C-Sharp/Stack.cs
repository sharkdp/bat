namespace StackImplementation
{
    internal class Stack<T>
    {
        private int _top;
        private const int Capacity = 4;
        private readonly T[] _stack = new T[Capacity];

        public Stack()
        {
            _top = -1;
        }

        private bool IsEmpty()
        {
            return _top < 0;
        }
        private bool IsFull()
        {
            return _top == Capacity - 1;
        }

        public void Peek()
        {
            System.Console.WriteLine(!IsEmpty() ? $"The topmost element is: {_stack[_top]}" : "The stack is empty.");
        }

        public T Pop()
        {
            return !IsEmpty() ? _stack[_top--] : default;
        }

        public void Push(T element)
        {
            if (!IsFull())
            {
                _stack[++_top] = element;
            }
            else
            {
                System.Console.WriteLine("Cannot push - the stack is full.");
            }
        }

        public override string ToString()
        {
            if (IsEmpty())
            {
                return "The stack is empty.";
            }

            var depiction = "";

            for (var index = 0; index < _top; index++)
            {
                depiction += _stack[index].ToString() + ' ';
            }

            depiction += _stack[_top].ToString();

            return $"Stack: [{depiction}]";
        }
    }
}
