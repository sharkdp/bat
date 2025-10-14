[38;2;253;151;31m#[0m[38;2;253;151;31m [0m[38;2;253;151;31mTypescript test[0m

[38;2;255;255;255m```[0m[38;2;190;132;255mtypescript[0m
[38;2;255;255;255menum Status {[0m
[38;2;255;255;255m    Pending,[0m
[38;2;255;255;255m    InProgress,[0m
[38;2;255;255;255m    Completed,[0m
[38;2;255;255;255m}[0m

[38;2;255;255;255minterface Task {[0m
[38;2;255;255;255m    id: number;[0m
[38;2;255;255;255m    title: string;[0m
[38;2;255;255;255m    status: Status;[0m
[38;2;255;255;255m    assignee?: string;[0m
[38;2;255;255;255m}[0m

[38;2;255;255;255mclass TaskManager<T extends Task> {[0m
[38;2;255;255;255m    private tasks: T[] = [];[0m

[38;2;255;255;255m    addTask(task: T): void {[0m
[38;2;255;255;255m        this.tasks.push(task);[0m
[38;2;255;255;255m    }[0m

[38;2;255;255;255m    getTasksByStatus(status: Status): T[] {[0m
[38;2;255;255;255m        return this.tasks.filter(task => task.status === status);[0m
[38;2;255;255;255m    }[0m

[38;2;255;255;255m    async fetchTasks(): Promise<T[]> {[0m
[38;2;255;255;255m        // Simulate async fetch[0m
[38;2;255;255;255m        return new Promise(resolve => setTimeout(() => resolve(this.tasks), 500));[0m
[38;2;255;255;255m    }[0m
[38;2;255;255;255m}[0m

[38;2;255;255;255m// Type guard[0m
[38;2;255;255;255mfunction isTask(obj: any): obj is Task {[0m
[38;2;255;255;255m    return typeof obj.id === 'number' && typeof obj.title === 'string';[0m
[38;2;255;255;255m}[0m

[38;2;255;255;255m// Usage[0m
[38;2;255;255;255mconst manager = new TaskManager<Task>();[0m
[38;2;255;255;255mmanager.addTask({ id: 1, title: "Write docs", status: Status.Pending });[0m
[38;2;255;255;255mmanager.addTask({ id: 2, title: "Review PR", status: Status.InProgress, assignee: "Alice" });[0m

[38;2;255;255;255m(async () => {[0m
[38;2;255;255;255m    const allTasks = await manager.fetchTasks();[0m
[38;2;255;255;255m    allTasks.forEach(task => {[0m
[38;2;255;255;255m        if (isTask(task)) {[0m
[38;2;255;255;255m            console.log(`Task #${task.id}: ${task.title} [${Status[task.status]}]`);[0m
[38;2;255;255;255m        }[0m
[38;2;255;255;255m    });[0m
[38;2;255;255;255m})();[0m

[38;2;255;255;255m// Type assertion[0m
[38;2;255;255;255mconst unknownValue: unknown = { id: 3, title: "Test", status: Status.Completed };[0m
[38;2;255;255;255mconst assertedTask = unknownValue as Task;[0m
[38;2;255;255;255mconsole.log(assertedTask.title);[0m
[38;2;255;255;255m```[0m
