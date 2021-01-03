<?php
#if this was md, i'll be a title

define("CONSTANT", 3.14);
echo CONSTANT;

//am i a comment
/*
	yes, and so am I too
*/


//variable declaration
$variable = "welcome";
$number = 2;
$float = 1.23;
$nothing = null;
$truth = true;
$lies = false;
$numberone = 2;
$numbertwo = 3;

//operators
$third = $numberone + $numbertwo;
$third = $numberone - $numbertwo;
$third = $numberone * $numbertwo;
$third = $numberone / $numbertwo;
$third = $numberone % $numbertwo;
$third = $numberone ** $numbertwo;
$numberone += $numbertwo;

echo $variable;

echo "You are $variable!";
echo "We are " . $variable ."s!";


if(($numberone >= 3 || $numberone <=2) && $numberone != 2.5){
	echo "what a number!!!";
}
if($numberone >= 3 and $numberone <=2 and $numberone != 2.5){
	echo "something is wrong, this is supposed to be impossible";
}


if ($number < 3){
	$languages = array("HTML", "CSS", "JS");
	print_r($languages);
	echo $languages[2];
	print $languages[$number];
}
elseif ($number == 3 ){
	function favMovie() {
		echo "JUMAJI";
		return true;
	}
	favMovie();
}
else {
	switch ($number) {
		case 4:
			echo "fours";
			break;
		default:
			echo "I dont know you";
	}
}

while($number <= 6 ){
	echo $number;
	$number++;
	$number += 1;
}

do {
	$number++;
} while ($number < 10);

for ($houses = 0; $houses <= 5; $housees++){
	break;
	echo "getting more houses";
}


class Person {
	public $name;
	public $age;
	
	function __construct($name){
		$this->name = $name;
	}
	
	function __destruct(){
		echo "On my way out";
	}
	
	function setName($name) {
		$this->name = $name;
	}
}

$doe = new Person("John Do");
$doe->setName('John Doe');





$ending = 2 > 3 ? "yep" : "nah";

?>
