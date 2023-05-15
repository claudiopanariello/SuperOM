// Class and methods to write OM files from SuperCollider
// Claudio Panariello
// cla.panariello@gmail.com
//
//
// Started on 12 June 2022

SuperOM {

	// Header of the OM file
	header {
		var header = ";fileheader
; (7.0 :inst 0 0 0 \"doc\" 183)
;endfileheader

(in-package :om)";
		^header.asString;
	}

	openPOLY {
		var template = "
(setf *instance-to-load*
(omng-make-new-instance
(make-instance 'poly
:voices
(list
";
		^template.asString;
	}


	closePOLY {
		var template = "))

\"instance\"))
";
		^template.asString;
	}

	closeVOICE {
		var template = "))
";
		^template.asString;
	}

	// create an Instance with

	makeInstanceVoice {
		arg rhythmTree = "1/4", metronome = 60;
		var template = "
(make-instance 'voice
:tree (mktree '(TREE) '(4 4))
:tempo METRONOME
:chords
(list
".replace("TREE", rhythmTree).replace("METRONOME", metronome);
		^template;
	}

	makeInstanceChord {
		arg midicent = 6000, channel = 1, velocity = 100;
		var template = "
(make-instance 'chord :lmidic '(MIDICENT) :lchan '(CHAN) :lvel '(VEL))".replace("MIDICENT", if(midicent.shape.isNil,{midicent},{midicent.arrayToCleanString})) //if midicent isn't an array, I use it as it is; otherwise I clean it and print it allprint it all
		.replace("CHAN", channel)
		.replace("VEL", velocity);
		^template;
	}

	makeInstanceVelExtra {
		arg dynamic = "f", midicent = 6000, channel = 1, velocity = 100;
		var template = "
(add-extra INSTANCECHORD
(make-instance 'vel-extra :deltax DELTAX :deltay DELTAY :dynamics :DYNAMIC)
nil t)
".replace("INSTANCECHORD", this.makeInstanceChord(midicent, channel, velocity))
		.replace("DELTAX", 0)
		.replace("DELTAY", 2)
		.replace("DYNAMIC", dynamic);
		^template;
	}

	//Takes midicents and magnitudes arrays and gives an array without the silent notes
	deleteSilence {
		arg midicents = [], magnitudes = [], threshold = -36;
		var result = midicents.copy;

		// shriking the array deleting notes relative to pauses
		magnitudes.do({|v, i|
			if(magnitudes[magnitudes.size-1-i]<threshold, {result.removeAt(magnitudes.size-1-i)})
		});
		^result;
	}

	// Taking the magnitudes array and try to make a RhythmTree
	makeRhythmTree {
		arg midicents = [], magnitudes = [], threshold = -36;
		var index = 0;
		var aux = Array.fill(magnitudes.size, 0);
		var result = "";
		var d = [];
		var md = midicents.copy;
		// shriking the array deleting notes relative to pauses
		magnitudes.do({|v, i|
			if(magnitudes[magnitudes.size-1-i]<threshold, {md.removeAt(magnitudes.size-1-i)})
		});

		// create a third array that contains both information (notes and rests)
		magnitudes.do({|v, i|
			if(v<threshold, {aux[i]=273.15.neg}, {aux[i] = md[index]; index = index+1;});
		});

		d = aux.groupOccurences;
		// and finally transforming this last array into the correct writing for the rhythm tree
		d.do({|v, i|
			result = result++"NUM/8 ".replace("NUM", v);
		});
		^result;
	}
	/*
	// In order to clean the given array, for example ["1/4","-1/4",-1/2"] must becomes: "1/4" "-1/4" "-1/2"
	arrayToCleanString {
	arg rhythmTree = "1/4";
	var result = "";
	rhythmTree.do({|v, i| result = result++v++" "; });
	^result;
	}
	*/

	// writing  full OM file: takes the path, the midicents array, the  magnitudes array,a threshold, and a microtone quantization in midicents
	writeOMfile {
		arg fileName, midicents = [], magnitudes = [], rhythmTree = nil, metronome = 60, quantization = 50, threshold = -36, dynamics = false;
		var outPath, outFile, midicentsTree, rhythmTreeFrac;
		var channels = Array.fill(midicents.size, 1);
		//outPath = PathName.new(path);
		//this.traverse(this);
		//outFile = File.new(outPath.standardizePath, "w");
		outFile = File.new(thisProcess.nowExecutingPath.dirname +/+ fileName, "w");

		outFile.write(this.header);
		outFile.write(this.openPOLY);

		// The following two lines are useful when 1-dim arrays are given:
		midicents = midicents.fixShape;
		magnitudes = magnitudes.fixShape;

		//if the magnitudes are nil, I create another array with all same velocities
		if(magnitudes[0][0].isNil,
			{ magnitudes = {{100}.dup(midicents.shape[1])}.dup(midicents.shape[0]); }
		);

		//Checking if the metronome is a sigle value or an array.
		//If the first case, it means that all the staffs need to have the same metronome, so it creates copies of metronome to match staff size.
		if(metronome.size==0, {metronome = metronome.dup(midicents.numRows)});

		// Iteration on the number of rows
		midicents.numRows.do({|n|
			// if there isn't rhythmTree I calculate one from the magnitudes, otherwise I just take the given rhythmTree
			if(rhythmTree==nil,
				{outFile.write(this.makeInstanceVoice(this.makeRhythmTree(midicents[n].round(quantization), magnitudes[n], threshold), metronome[n]));},
				{
					rhythmTreeFrac = rhythmTree.fixShape.toFractionString; //here IMPORTANT transition from floats to fraction string
					//outFile.write(this.makeInstanceVoice((this.arrayToCleanString(rhythmTreeFrac[n])), metronome));
					outFile.write(this.makeInstanceVoice(((rhythmTreeFrac[n])).arrayToCleanString, metronome[n]));

			});

			midicentsTree = this.deleteSilence(midicents[n].round(quantization), magnitudes[n], threshold).groupItems; //This method would group similar items, basically preventing ribattutos
			midicentsTree.do({|v, i|
				outFile.write(
					if(dynamics, {
						this.makeInstanceVelExtra(
							magnitudes[n][i].dbvelSmart.veldyn,
							v,
							channels[i],
							magnitudes[n][i].dbvelSmart
					)},
					{
						this.makeInstanceChord(
							v,
							channels[i],
							magnitudes[n][i].dbvelSmart
				);}))
			});
			outFile.write(this.closeVOICE);
		});

		outFile.write(this.closePOLY);
		outFile.close;
	}

}


// EOF