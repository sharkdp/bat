let letNumber = 1000;
const constNumber = 10;
var varNumber = -1234;
const constNegativeFloat = -1.23;
var tooMuch = Infinity;
nothing = null;
let listofthings = ["thing", 'thing2', `foo`, ["bar"]];

// Simple comment

/**
* #########
* Multiline
* comment
* #########
*/

let test;
for (let i = 0; i < constNumber; i++) {
	if (test) continue;
	else test += 1; // random things
}

while(test < 100 && typeof test === "number") {
	test = test > 30 ? test+5 : test+1;
}

function weatherSays(when=Date.now()) {
	return "rain";
}

const thereAreClouds = true;
const cloudsCount = 20;

switch(weatherSays(Date.now())) {
	case 'rain':
		break;
	case 'sun':
	default:
		break;
}

let rain = false;
if ((thereAreClouds && cloudsCount >= 20) || weatherSays() === "rain") {
	rain = false;
} else if (thereAreClouds && weatherSays() == "rain") { // oh no, unsafe two equals checking!
	rain = true;
} else {
	rain = !!cloudsCount;
}

class Forecast {
	constructor(where, isGonnaRainA=true, isGonnaRainB=false, isGonnaRainC=false, ...randomArgs) {
		this.station = {
			location: [where.x, where.y, where.z],
			surroundings: {
				zoneA: {
					location: [1, 2, 3],
					isGonnaRain: isGonnaRainA
				},
				zoneB: {
					location: [-1, 2, 2],
					isGonnaRain: isGonnaRainB
				},
				zoneC: {
					location: [-2, 0, 0],
					isGonnaRainC: isGonnaRainC
				},
			}
		};
	}
	
	async getLocalPrevisions() {
		const rainZones = [this.station.surroundings.zoneA.isGonnaRain, this.station.surroundings.zoneB.isGonnaRain, this.station.surroundings.zoneC.isGonnaRain];
		return await rainZones.filter(z => !!z).length > (rainZones.length / 2);
	}
	
	communicatePrevisions(isGonnaRain=undefined) {
		if (isGonnaRain) console.log("Take the umbrella.");
	}
	
	destroy() {
		delete this.station;
	}
	
	static startHiring() {
		console.log("We're looking for weather presenters.");
		console.log("A lot of presenters came. Hiring stops.");
	}
	
	/* This forecasting station is magic. It can generate rain, but this method is secret because it's a generator function - nobody uses them! */
	* generateRainInZoneC(clouds=[1, 2, 3]) {
		this.station.surroundings.zoneC.isGonnaRain = true;
		const makeRain = () => { return "raining!"; };
		
		yield clouds; // first, keeps clouds
		do {
			console.log(makeRain());
			yield clouds.pop(); // then all clouds do rain
		} while(clouds.length >= 1);
	}
}

Forecast.startHiring();
const forecasting = new Forecast([3, 3, 3]);
(async() => {
	const raining = forecasting.generateRainInZoneC();
	raining.next();
	forecasting.communicatePrevisions(await forecasting.getLocalPrevisions());
	raining.next();
	raining.next();
	raining.return("stop!");
	forecasting.destroy();
})();
