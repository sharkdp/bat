# Typescript test

```typescript
enum Status {
    Pending,
    InProgress,
    Completed,
}

interface Task {
    id: number;
    title: string;
    status: Status;
    assignee?: string;
}

class TaskManager<T extends Task> {
    private tasks: T[] = [];

    addTask(task: T): void {
        this.tasks.push(task);
    }

    getTasksByStatus(status: Status): T[] {
        return this.tasks.filter(task => task.status === status);
    }

    async fetchTasks(): Promise<T[]> {
        // Simulate async fetch
        return new Promise(resolve => setTimeout(() => resolve(this.tasks), 500));
    }
}

// Type guard
function isTask(obj: any): obj is Task {
    return typeof obj.id === 'number' && typeof obj.title === 'string';
}

// Usage
const manager = new TaskManager<Task>();
manager.addTask({ id: 1, title: "Write docs", status: Status.Pending });
manager.addTask({ id: 2, title: "Review PR", status: Status.InProgress, assignee: "Alice" });

(async () => {
    const allTasks = await manager.fetchTasks();
    allTasks.forEach(task => {
        if (isTask(task)) {
            console.log(`Task #${task.id}: ${task.title} [${Status[task.status]}]`);
        }
    });
})();

// Type assertion
const unknownValue: unknown = { id: 3, title: "Test", status: Status.Completed };
const assertedTask = unknownValue as Task;
console.log(assertedTask.title);
```
