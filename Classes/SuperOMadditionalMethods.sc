// Claudio Panariello
// cla.panariello@gmail.com

//Extending functionality with some method required for the OMClass

+ SequenceableCollection {
	toFractionString { ^this.performUnaryOp('toFractionString') }
	cpsmidicents { ^this.performUnaryOp('cpsmidicents') }
	arrayToCleanString { ^this.performUnaryOp('arrayToCleanString') }
	dbvel { ^this.performUnaryOp('dbvel') }
	dbvelSmart { ^this.performUnaryOp('dbvelSmart') }
	veldb { ^this.performUnaryOp('veldb') }
	veldyn { ^this.performUnaryOp('veldyn') }
}


+ Array {
	// To "clean" a given array, for example ["1/4","-1/4",-1/2"] must becomes: "1/4" "-1/4" "-1/2"
	arrayToCleanString {
		arg rhythmTree = this;
		var result = "";
		rhythmTree.do({|v, i| result = result++v++" "; });
		^result;
	}

	// It groups similar consecutive items and returns an array with them.
	// This is useful for example for the OM stuff
	groupItems {
		var index = 1, result = [];
		this.do({|v, i|
			if(v!=this[i+1], {
				result = result.add(v)
			})
		});
		^result;
	}

	// It groups similar consecutive items and returns an array with the occurences.
	// It takes also a threshold so to group items less than the threshold with a negative sign.
	// This is useful for example for the OM stuff
	groupOccurences {
		arg threshold = -21;
		var index = 1, result = [];
		this.do({|v, i|
			if(v==this[i+1], {
				index = index+1;},
			{
				if(v<threshold, {index=index.neg});
				result=result++index;
				index = 1;
			});
		});
		^result;
	}

	// To get the number of rows of an array. This is because I want numRows(1-dimensional Array) = 1
	numRows {
		var result = this.size;
		if(this.shape[1]==nil, {result = 1;} );
		^result;
	}

	// If I have a 1-dim array, I encapsulate it, so the shape changes from [n] to [1, n]
	fixShape{
		var result = this;
		if(this.shape[1]==nil, {result = [result];} );
		^result;
	}

	// Find the maximum Item in a matrix.
	// If it is an array, it works as .maxItem
	maxItem2D {
		var input, newArray, result;
		input = this;
		newArray = Array.newClear(input.size);
		input.size.do({|i| newArray[i] = input[i].maxItem});
		result = newArray.maxItem;

		^result;
	}
}

+ Rest {
	//Transforming a Rest into a fraction string (useful for the RhythmTree)
	toFractionString {
		var float = this;
		var base = 1e8, mul, greatCommDiv, aux = (1..51), denominator, numerator, sign = "-", result;
		float = float.dur;
		mul = float*base;
		if(mul.frac==0,
			{ //finite decimals
				greatCommDiv = mul.asInteger.gcd(base.asInteger);
				numerator = (mul/greatCommDiv).asInteger;
				denominator = (base/greatCommDiv).asInteger;
			},
			{ //non finite decimals
				denominator = ((aux*float).frac.minIndex+1).asInteger;
				numerator = (float*denominator).asInteger;
			}
		);
		result = sign++numerator.asString++"/"++denominator.asString;
		^result;
	}
}

+ SimpleNumber {
	//Transforming a float into a fraction string (useful for the RhythmTree)
	toFractionString {
		var float = this;
		var base = 1e8, mul, greatCommDiv, aux = (1..51), denominator, numerator, sign, result;
		if(float>0, {sign=""}, {float = abs(float); sign = "-"}); //storing the sign of the number
		mul = float*base;
		if(mul.frac==0,
			{ //finite decimals
				greatCommDiv = mul.asInteger.gcd(base.asInteger);
				numerator = (mul/greatCommDiv).asInteger;
				denominator = (base/greatCommDiv).asInteger;
			},
			{ //non finite decimals
				denominator = ((aux*float).frac.minIndex+1).asInteger;
				numerator = (float*denominator).asInteger;
			}
		);
		result = sign++numerator.asString++"/"++denominator.asString;
		^result;
	}

	//Transforming bpm to duration in seconds
	tempodur {
		var tempo, beatdur;
		tempo = this; //"this" is the receiver of the method
		beatdur = 60/tempo;
		^beatdur; // this is the value returned
	}

	//Just a frequency expressed in midicents
	cpsmidicents {
		var freq, midicents;
		freq = this;
		midicents = freq.cpsmidi*100;
		^midicents;
	}

	/*
	Vel| Dyn.|   dB |rev.dB|   % |
	+----+-----+------+------+-----+
	| 127| fff |   0.0| 57.6 | 100 |
	| 112| ff  |  -2.2| 55.4 |  86 |
	|  96| f   |  -4.9| 52.7 |  71 |
	|  80| mf  |  -8.0| 49.6 |  57 |
	|  64| mp  | -11.9| 45.7 |  44 |
	|  48| p   | -16.9| 40.7 |  31 |
	|  32| pp  | -23.9| 33.7 |  19 |
	|  16| ppp | -36.0| 21.6 |   8 |
	|   0|-off-| -57.6|    0 |   0 |
	*/

	//decibel to midi velocity
	dbvel {
		var db, velocity;
		db = this;
		velocity = case
		{ db == 0.0 } { 127 }
		{ (db>=2.2.neg)&&(db<0.0) } { 112 }
		{ (db>=4.9.neg)&&(db<2.2.neg) } { 96 }
		{ (db>=8.0.neg)&&(db<4.9.neg) } { 80 }
		{ (db>=11.9.neg)&&(db<8.0.neg) } { 64 }
		{ (db>=16.9.neg)&&(db<11.9.neg) } { 48 }
		{ (db>=23.9.neg)&&(db<16.9.neg) } { 32 }
		{ (db>=36.neg)&&(db<23.9.neg) } { 16 }
		{ db < 36.neg} { 0 };
		^velocity;
	}

	//decibel to midi velocity SMART
	// TRICK: when decibel > 0, then the input is interpeted as velocity, and then .dbvelocity gives back the input as it is
	dbvelSmart {
		var db, velocity;
		db = this;
		velocity = case
		{ db > 0.0 } { this }
		{ db == 0.0 } { 127 }
		{ (db>=2.2.neg)&&(db<0.0) } { 112 }
		{ (db>=4.9.neg)&&(db<2.2.neg) } { 96 }
		{ (db>=8.0.neg)&&(db<4.9.neg) } { 80 }
		{ (db>=11.9.neg)&&(db<8.0.neg) } { 64 }
		{ (db>=16.9.neg)&&(db<11.9.neg) } { 48 }
		{ (db>=23.9.neg)&&(db<16.9.neg) } { 32 }
		{ (db>=36.neg)&&(db<23.9.neg) } { 16 }
		{ db < 36.neg} { 0 };
		^velocity;
	}

	//midi velocity to decibel
	veldb {
		var db, velocity;
		velocity = this;
		db = case
		{ velocity == 127 } { 0.0 }
		{ (velocity>=112)&&(velocity<127) } { 2.2.neg }
		{ (velocity>=96)&&(velocity<112) } { 4.9.neg }
		{ (velocity>=80)&&(velocity<96) } { 8.0.neg }
		{ (velocity>=64)&&(velocity<80) } { 11.9.neg }
		{ (velocity>=48)&&(velocity<64) } { 16.9.neg }
		{ (velocity>=32)&&(velocity<48) } { 23.9.neg }
		{ (velocity>=16)&&(velocity<32) } { 36.neg }
		{ velocity < 16} { -inf };
		^db;
	}

	//midi velocity to music dynamics
	veldyn {
		var db, velocity;
		velocity = this;
		db = case
		{ velocity == 127 } { "fff" }
		{ (velocity>=112)&&(velocity<127) } { "ff" }
		{ (velocity>=96)&&(velocity<112) } { "f" }
		{ (velocity>=80)&&(velocity<96) } { "mf" }
		{ (velocity>=64)&&(velocity<80) } { "mp" }
		{ (velocity>=48)&&(velocity<64) } { "p" }
		{ (velocity>=32)&&(velocity<48) } { "pp" }
		{ (velocity>=16)&&(velocity<32) } { "ppp" }
		{ velocity < 16} { "ppppp" };
		^db;
	}
}

//EOF