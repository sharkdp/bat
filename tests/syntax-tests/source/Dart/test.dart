/* array sorting alogorithm */
int partition(List list, int low, int high) {
  if (list == null || list.length == 0) return 0;
  int pivot = list[high];
  int i = low - 1;

  void swap(List list, int i, int j) {
    int temp = list[i];
    list[i] = list[j];
    list[j] = temp;
  }

  for (int j = low; j < high; j++) {
    if (list[j] <= pivot) {
      i++;
      swap(list, i, j);
    }
    swap(list, i + 1, high);
    return i + 1;
  }
}

void quickSort(List list, int low, int high) {
  if (low < high) {
    int pi = partition(list, low, high);
    quickSort(list, low, pi - 1);
    quickSort(list, pi + 1, high);
  }
}

void merge(List list, int leftIndex, int middleIndex, int rightIndex) {
  int leftSize = middleIndex - leftIndex + 1;
  int rightSize = rightIndex - middleIndex;

  List leftList = new List(leftSize);
  List rightList = new List(rightSize);

  for (int i = 0; i < leftSize; i++) leftList[i] = list[leftIndex + i];
  for (int j = 0; j < rightSize; j++) rightList[j] = list[middleIndex + j + 1];

  int i = 0, j = 0;
  int k = leftIndex;

  while (i < leftSize && j < rightSize) {
    if (leftList[i] <= rightList[j]) {
      list[k] = leftList[i];
      i++;
    } else {
      list[k] = rightList[j];
      j++;
    }
    k++;
  }

  while (i < leftSize) {
    list[k] = leftList[i];
    i++;
    k++;
  }

  while (j < rightSize) {
    list[k] = rightList[j];
    j++;
    k++;
  }
}

void mergeSort(List list, int leftIndex, int rightIndex) {
  if (leftIndex < rightIndex) {
    int middleIndex = (rightIndex + leftIndex) ~/ 2;

    mergeSort(list, leftIndex, middleIndex);
    mergeSort(list, middleIndex + 1, rightIndex);

    merge(list, leftIndex, middleIndex, rightIndex);
  }
}

/* variables */
var name = 'Voyager I';
var year = 1977;
var antennaDiameter = 3.7;
var flybyObjects = ['Jupiter', 'Saturn', 'Uranus', 'Neptune'];
var image = {
  'tags': ['saturn'],
  'url': '//path/to/saturn.jpg'
};

/*classes */
class Spacecraft {
  String name;
  DateTime launchDate;
  Spacecraft(this.name, this.launchDate) {}

  // Named constructor that forwards to the default one.
  Spacecraft.unlaunched(String name) : this(name, null);

  int get launchYear => launchDate?.year;

  void describe() {
    print('Spacecraft: $name');
    if (launchDate != null) {
      int years = DateTime.now().difference(launchDate).inDays ~/ 365;
      print('Launched: $launchYear ($years years ago)');
    } else {
      print('Unlaunched');
    }
  }
}

/* Mixins */
class PilotedCraft extends Spacecraft with Piloted {
  // ···
}

/* Interfaces and abstract classes */
class MockSpaceship implements Spacecraft {
  // ···
}

/* async */
Future<void> printWithDelay(String message) {
  return Future.delayed(const Duration(seconds: 2)).then((_) {
    print(message);
  });
}

Stream<String> report(Spacecraft craft, Iterable<String> objects) async* {
  for (var object in objects) {
    await Future.delayed(const Duration(seconds: 2));
    yield '${craft.name} flies by $object';
  }
}
