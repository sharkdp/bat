class Person {
// We can define class property here
var age  = 25
// Implement Class initializer. Initializers are called when a new object of this class is created
init() { 
   print(“A new instance of this class Person is created.”) 
 } 
} 
// We can now create an instance of class Person - an object - by putting parentheses after the class name
let personObj =  Person()
// Once an instance of Person class is created we can access its properties using the dot “.” syntax.
print(“This person age is \(personObj.age)”)

import Foundation
class Friend : Comparable {
    let name : String
    let age : Int
    
    init(name : String, age: Int) {
        self.name = name
        self.age = age
    }
}
func < (lhs: Friend, rhs: Friend) -> Bool {
    return lhs.age < rhs.age }; func > (lhs: Friend, rhs: Friend) -> Bool {
    return lhs.age > rhs.age
}
func == (lhs: Friend, rhs: Friend) -> Bool {
    var returnValue = false
    if (lhs.name == rhs.name) && (lhs.age == rhs.age)
    {
        returnValue = true
    }
    return returnValue
}

  let friend1 = Friend(name: "Sergey", age: 35)
        let friend2 = Friend(name: "Sergey", age: 30)
        
        print("Compare Friend object. Same person? (friend1 == friend2)")

func sayHelloWorld() {
    print("Hello World")
}
// Call function
sayHelloWorld()

func printOutFriendNames(names: String...)  {
  
    for name in names {
 
        print(name)
    }
 
}
// Call the printOutFriendNames with two parameters
printOutFriendNames("Sergey", "Bill")
// Call the function with more parameters
printOutFriendNames("Sergey", "Bill", "Max")

let simpleClosure = {
    print("From a simpleClosure")
}
// Call closure
simpleClosure() 

let fullName = { (firstName:String, lastName:String)->String in
    return firstName + " " + lastName
}
// Call Closure
let myFullName = fullName("Sergey", "Kargopolov")
print("My full name is \(myFullName)")

let myDictionary = [String:String]()
// Another way to create an empty dictionary
let myDictionary2:[String:String] = [:]
// Keys in dictionary can also be of type Int
let myDictionary3 = [Int:String]()

var myDictionary = ["first_name": "Sergey", "last_name": "Kargopolov"]
// print to preview
print(myDictionary)
// Add a new key with a value
myDictionary["user_id"] = "f5h7ru0tJurY8f7g5s6fd"
// We should now have 3 key value pairs printed
print(myDictionary)

var myDictionary = ["first_name": "Sergey", "last_name": "Kargopolov"]
// Loop through dictionary keys and print values
for (key,value) in myDictionary {
    print("\(key) = \(value)")
}
 class Friend {
    let name : String
    let age : Int
    
    init(name : String, age: Int) {
        self.name = name
        self.age = age
    }
}

 var friends:[Friend] = []
        
        let friend1 = Friend(name: "Sergey", age: 30)
        let friend2 = Friend(name: "Bill", age: 35)
        let friend3 = Friend(name: "Michael", age: 21)
        
        friends.append(friend1)
        friends.append(friend2)
        friends.append(friend3)
        
        printFriends(friends: friends)
        
        // Get sorted array in descending order (largest to the smallest number)
        let sortedFriends = friends.sorted(by: { $0.age > $1.age })
        printFriends(friends: sortedFriends)
        
        // Get sorted array in ascending order (smallest to the largest number)
        let sortedFriendsAscendingOrder = friends.sorted(by: { $0.age < $1.age })
        printFriends(friends: sortedFriendsAscendingOrder)


 func printFriends(friends: [Friend])
    {
        for friendEntry in friends {
            print("Name: \(friendEntry.name), age: \(friendEntry.age)")
        }
    }

import UIKit
class ViewController: UIViewController  {
override func viewDidLoad() {
    super.viewDidLoad()
}
override func viewWillAppear(_ animated: Bool) {
    super.viewWillAppear(animated)
    
    // Create destination URL 
    let documentsUrl:URL =  FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first as URL!
    let destinationFileUrl = documentsUrl.appendingPathComponent("downloadedFile.jpg")
    
    //Create URL to the source file you want to download
    let fileURL = URL(string: "https://s3.amazonaws.com/learn-swift/IMG_0001.JPG")
    
    let sessionConfig = URLSessionConfiguration.default
    let session = URLSession(configuration: sessionConfig)
 
    let request = URLRequest(url:fileURL!)
    
    let task = session.downloadTask(with: request) { (tempLocalUrl, response, error) in
        if let tempLocalUrl = tempLocalUrl, error == nil {
            // Success
            if let statusCode = (response as? HTTPURLResponse)?.statusCode {
                print("Successfully downloaded. Status code: \(statusCode)")
            }
            
            do {
                try FileManager.default.copyItem(at: tempLocalUrl, to: destinationFileUrl)
            } catch (let writeError) {
                print("Error creating a file \(destinationFileUrl) : \(writeError)")
            }
            
        } else {
            print("Error took place while downloading a file. Error description: %@", error?.localizedDescription);
        }
    }
    task.resume()
    
  }
}

  do {
            
            // Convert JSON Object received from server side into Swift NSArray.
            // Note the use "try"
            if let convertedJsonIntoArray = try JSONSerialization.JSONObjectWithData(data!, options: []) as? NSArray {
            }
            
        } catch let error as NSError {
            print(error.localizedDescription)
        }

DispatchQueue.global(qos: .userInitiated).async {
            // Do some time consuming task in this background thread
            // Mobile app will remain to be responsive to user actions
            
            print("Performing time consuming task in this background thread")
            
           DispatchQueue.main.async {
                // Task consuming task has completed
                // Update UI from this block of code
                print("Time consuming task has completed. From here we are allowed to update user interface.")
            }
        }

import UIKit
class ViewController: UIViewController {
    
    override func viewDidLoad() {
        super.viewDidLoad()
        // Do any additional setup after loading the view, typically from a nib.
        
        let button = UIButton(type: UIButtonType.system) as UIButton
        
        let xPostion:CGFloat = 50
        let yPostion:CGFloat = 100
        let buttonWidth:CGFloat = 150
        let buttonHeight:CGFloat = 45
        
        button.frame = CGRect(x:xPostion, y:yPostion, width:buttonWidth, height:buttonHeight)
        
        button.backgroundColor = UIColor.lightGray
        button.setTitle("Tap me", for: UIControlState.normal)
        button.tintColor = UIColor.black
        button.addTarget(self, action: #selector(ViewController.buttonAction(_:)), for: .touchUpInside)
        
        self.view.addSubview(button)
    }
    
    func buttonAction(_ sender:UIButton!)
    {
        print("Button tapped")
    }
    
    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Dispose of any resources that can be recreated.
    }
    
    
}

import UIKit
class ViewController: UIViewController {
    
    override func viewDidLoad() {
        super.viewDidLoad()
        
        //Create Activity Indicator
        let myActivityIndicator = UIActivityIndicatorView(activityIndicatorStyle: UIActivityIndicatorViewStyle.gray)
        
        // Position Activity Indicator in the center of the main view
        myActivityIndicator.center = view.center
        
        // If needed, you can prevent Acivity Indicator from hiding when stopAnimating() is called
        myActivityIndicator.hidesWhenStopped = false
        
        // Start Activity Indicator
        myActivityIndicator.startAnimating()
        
        // Call stopAnimating() when need to stop activity indicator
        //myActivityIndicator.stopAnimating()
        
        
        view.addSubview(myActivityIndicator)
    }
    
    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
    }
    
}





