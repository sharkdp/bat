#include <bits/stdc++.h> // includes most of the useful libraries
#include "mainwindow.h"

using namespace std;

/* This C program was submitted to help bat
 * with its syntax highlighting tests
 */

bool test_function(int x, int y)
{
    return (x == y || x + y == x * y);
}

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent), ui(new Ui::MainWindow)
{
    ui->setupUi(this);
    resize(560, 420);
    // Image menu
    connect(ui->actionOpen, &QAction::triggered, this, &MainWindow::open);
    MainWindow::~MainWindow()
    {
        delete ui;
    }
    int y = ui->graphicsView->size().height() - image.height();
    int x = ui->graphicsView->size().width() - image.width();

    void MainWindow::showImage()
    {
        // Reset scale
        ui->graphicsView->resetMatrix();

        statusBar()->showMessage(QString("%1 %2x%3px %4 kB").arg(file.fileName()).arg(image.width()).arg(image.height()).arg(QFile(currentFile).size() / 1000.0));
    }
}

int main()
{
    int tc;
    cin >> tc;
    while (tc--) // for each test case
    {
        int n;
        cin >> n; // take n
        vector<pair<int, string>> v;

        int a[n], b[n];

        for (int i = 0; i < n; i++)
            cin >> a[i]; // take arrays
        for (int i = 0; i < n; i++)
            cin >> b[i];

        priority_queue<int> pq; // inbuilt data structure - max heap (available in the bits/stdc++)

        for (int i = 0; i < n; i++)
            pq.push(a[i]); // push elements into heap

        for (int i = 0; i < n; i++) // for each element in B
        {
            int top_ele = pq.top();  // max element in heap
            pq.pop();                // deletion
            pq.push(top_ele ^ b[i]); // push after operation
        }

        for (int i = n - 1; i >= 0; i--) // restore A in ascending order
        {
            a[i] = pq.top();
            pq.pop();
        }

        for (int i = 0; i < n; i++)
            cout << a[i] << " "; // print A

        cout << endl;
    }
    return 0;
}
