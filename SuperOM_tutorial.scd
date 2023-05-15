// Claudio Panariello
// cla.panariello@gmail.com

/*
███████╗██╗   ██╗██████╗ ███████╗██████╗  ██████╗ ███╗   ███╗    ████████╗██╗   ██╗████████╗ ██████╗ ██████╗ ██╗ █████╗ ██╗
██╔════╝██║   ██║██╔══██╗██╔════╝██╔══██╗██╔═══██╗████╗ ████║    ╚══██╔══╝██║   ██║╚══██╔══╝██╔═══██╗██╔══██╗██║██╔══██╗██║
███████╗██║   ██║██████╔╝█████╗  ██████╔╝██║   ██║██╔████╔██║       ██║   ██║   ██║   ██║   ██║   ██║██████╔╝██║███████║██║
╚════██║██║   ██║██╔═══╝ ██╔══╝  ██╔══██╗██║   ██║██║╚██╔╝██║       ██║   ██║   ██║   ██║   ██║   ██║██╔══██╗██║██╔══██║██║
███████║╚██████╔╝██║     ███████╗██║  ██║╚██████╔╝██║ ╚═╝ ██║       ██║   ╚██████╔╝   ██║   ╚██████╔╝██║  ██║██║██║  ██║███████╗
╚══════╝ ╚═════╝ ╚═╝     ╚══════╝╚═╝  ╚═╝ ╚═════╝ ╚═╝     ╚═╝       ╚═╝    ╚═════╝    ╚═╝    ╚═════╝ ╚═╝  ╚═╝╚═╝╚═╝  ╚═╝╚══════╝

*/


/*
The SuperOM is a SuperCollider (SC) class that produces an OpenMusic (OM) file in output, thus bridging the two softwares.

OpenMusic is a computer-assisted composition software designed at IRCAM. It can be downloaded for free from https://openmusic-project.github.io/openmusic/

The main aim behind this class is to allow SC users to generate music scores, that is OM files with extension ".omi".
Eventually, the .omi file can be manipulated by OM users. Moreover, this workflow allows also to use the OM functionality of XML export, thus having a file that can be opened and edited in the most common notation softwares.

The main method of SuperOM is .writeOMfile, which takes the following arguments:
- file name output with .omi extension. The output will be produced in the same folder where the SC file is.
- array of midicents notes. Chords can be specified with additional brackets, e.g. [n, n, [n, n], n, ...]. The midicents array can also be an array of array [[][][]...]. In this case the rows are interpreted as subsequent separated staves.
- array of velocities, i.e. values 0-127. This can be expressed in decibels, the SuperOM will take care of interpreting the given array in the correct way. This must match the size of the midicents array. If no magnitudes are given, then all the notes are automatically set to velocity = 100.
- array of rhythms, given as floats (or fractions). This follows the same OM criterion to specify rhythms, that is a positive value represents the duration of the note, and a negative values represents a pause. A rest, however, can be also specified using SC's operand Rest(). If this argument is "nil", then the magnitudes array is used as a source for a rhythm tree (see more details below).
- metronome. If only one value is given, the metronome will be the same for all the staves. In alternative, an array of metronomes can be specified, matching the rows size  of the midicents' array. Default valus is 60.
- quantization of the notes (expressed in MIDICENTS). Default value is 50 (i.e. quarter-tone).
- threshold on the magnitudes (levels below this will be considered as silence, i.e. pauses). Default value is -36 db.
– dynamics flag. This can be true of false. If true, the output file will also show the notes' magnitudes as music dynamics (i.e. from "ppp" to "fff"). Default value is "false".

The .omi file produced by the SuperOM can be dragged and dropped into an OM patch and opened from there.
You can use a POLY object to enable the XLM export:
- create the POLY obect by double-clicking in the OM patch and typing "POLY".
- connect the imported file to <self> input of POLY and evaluate it by selecting POLY it and clicking "v".
- click "m" if you want to show the content of POLY in the window.
- right-click on the POLY object to export the content as an XLM file.

*/



////////////////////////////////
////                        ////
////        EXAMPLES        ////
////                        ////
////////////////////////////////



// One staff with some notes and chords and a rhythm tree
(
var pitches = [6000, [6200, 6550, 6800], 7000];
var rhythm = [1/2, -1/6, 2/6, -2/5, 3/5];

o = SuperOM.new;
o.writeOMfile("example0.omi", pitches, rhythmTree: rhythm, metronome: 144);
)



//A chromatic scale with eight-tones starting from C4, one staff, all 32th notes
(
var pitches = (6000, 6025..7200);
var rhythm = {1/32}.dup(pitches.size);

o = SuperOM.new;
o.writeOMfile("example1.omi", pitches, rhythmTree: rhythm, metronome: 144, quantization: 25);
)



// As before, but adding random magnitudes and printing them in the score
(
var pitches = (6000, 6025..7200);
var mags = {rrand(-18, -3)}.dup(pitches.size);
var rhythm = {1/32}.dup(pitches.size);

o = SuperOM.new;
o.writeOMfile("example2.omi", pitches, magnitudes: mags, rhythmTree: rhythm, metronome:144, quantization: 25, dynamics:true);
)



// 5 staves with different chromatic scales, rhythms with pauses chosen by a given set, 5 different metronomes
(
var staves = 5;
var pitches = {(6000, 6050..7200)+(rrand(-5, 5)*100)}.dup(staves);
var rhythm = {{[1/4, 1/8, 1/16].choose*[-1, 1].choose}.dup(pitches.shape[1]*2)}.dup(staves);
var metronomes = {rrand(102, 144)}.dup(staves);

o = SuperOM.new;
o.writeOMfile("example3.omi", pitches, rhythmTree: rhythm, metronome: metronomes);
)



// 8 staves with random frequencies taken from an harmonic series (and translated into midicentes with the .cpsmidicents method), random magnitudes and random rhythms quantized to 1/32.
(
var notes = 200, staves = 8;
var pitches = {{Array.fill(24, {|i|(i+1*rrand(50, 51))}).choose}.dup(notes)}.dup(staves).cpsmidicents;
var mags = {{rrand(-18, -3)}.dup(notes)}.dup(staves);
var rhythm = {{rrand(0.1, 1).softRound(1/32, 0, 1)}.dup(notes)}.dup(staves);

o = SuperOM.new;
o.writeOMfile("example4.omi", pitches, mags, rhythm, 84, 25, -36, true);
)



// This example shows how midicents and magnitudes work together to create a final score.
// This can be handy when you want to create a score from a series of frequencies and magnitudes coming from spectral analysis.
// The general strategy is that subsequent notes have same freq and magnitude are grouped.
// If they have different mags, instead, they are left as they are.
// Finally, the mags below the threshold are interpreted as silence, i.e. the notes relative to those silences are transformed into pauses.
(
var freqs = {rrand(400, 500)}.dup(50).cpsmidicents;
var mags = {[-18, -12, -9, -6].choose}.dup(freqs.size);
freqs.postln;
mags.postln;
o = SuperOM.new;
o.writeOMfile("example5.omi", freqs, magnitudes: mags, rhythmTree: nil, metronome:144, quantization: 100, threshold: -12, dynamics:true);
)



// Writing a score from a pattern.
// In order to do that, the events must be stored into arrays in a convenient way.
(
var length = 50, pitches, rhythm;
p = Pbind(
	\midinote, Pxrand([60, 62, 64, 66, 68, 70], inf),
	\dur, Prand([1/16, 1/8, Rest(1/16), Rest(1/8)], inf),
);

//p.play;

e = p.asStream;
pitches = length.collect({e.next(()).midinote})*100;
rhythm = length.collect({e.next(()).dur});

o = SuperOM.new;
o.writeOMfile("example6.omi", pitches, rhythmTree: rhythm, metronome:144);
)



// Writing a score from two longer patterns.
(
var melody, melodyOct, melRhythm, bass, bassOct, bassRhythm;
var length = 16*12, e1, e2, pitches1, rhythm1, pitches2, rhythm2;

// Pattern
a = [74, 69, 68, 67, 65, 62, 65, 67];
melody = (62!2)++a++(60!2)++a++(59!2)++a++(58!2)++a;
melodyOct = [melody, melody+12].flop;
melRhythm = [1/16, 1/16, 1/8, 3/16, 1/8, 1/8, 1/8, 1/16, 1/16, 1/16];
bass = (50!10)++(48!10)++(47!10)++(46!4)++(48!6);
bassOct = [bass, bass-12].flop;
bassRhythm = [1/8, 1/8, 1/16, 1/8, 1/8, 1/8, 1/16, 1/16, 1/16, 1/8];

p = Pbind(
	\midinote, Pseq(melody, 2)++Pseq(melodyOct, inf),
	\dur, Pseq(melRhythm, inf));
q = Pbind(
	\midinote, Pseq(bass, 1)++Pseq(bassOct, inf),
	\dur, Pseq([Rest(1/16)], 64)++Pseq(bassRhythm, inf));

/*(
t = TempoClock.default;
t.tempo = 30/60;
p.play(t);
q.play(t);
)*/

// Collecting the events in arrays containing pitches and rhythm information
e1 = p.asStream;
pitches1 = length.collect({e1.next(()).midinote})*100;
e1.reset;
rhythm1 = length.collect({e1.next(()).dur});

e2 = q.asStream;
pitches2 = length.collect({e2.next(()).midinote})*100;
e2.reset;
rhythm2 = length.collect({e2.next(()).dur});

// Using the arrays to write a score
o = SuperOM.new;
o.writeOMfile("example7.omi", [pitches1, pitches2], rhythmTree: [rhythm1, rhythm2], metronome:120);
)


///////////////////////////////////////////
////                                   ////
////        CURRENT LIMITATIONS        ////
////                                   ////
///////////////////////////////////////////

// One current limitation (for the moment) is how a midicent array is presented: when it starts with a chord, it needs some extra brackets in order to get properly recognized by the class
(
var pitches = {|i|200*i+[5500, 5900, 6200]}.dup(18);
var rhythm = {1/6}.dup(18);

o = SuperOM.new;
o.writeOMfile("example8.omi", [pitches], rhythmTree: rhythm, metronome: 144);
)



// Another potential limitation is about combining midicents staves that have different lengths: as a matter of fact, midicents arrays should all have the same length, in order to create a final rectangular matrix.
// One workaround is to fill the shorter arrays with zeros (a sort of zero padding), thus matching the size of the longest array: these additional 0 pitches will be ignored as long as the rhythm tree doesn't contain a rhythm information for them.
(
var pitches1 = [7200, 7400, 7500, 7600];
var rhythm1 = [1/6, -2/6, 1/4, 1/4, 1/4];

var pitches2 = [6000, 6200, 6550, 6800, 7000, 6800, 5300, 5625, 6378, 6740];
var rhythm2 = [1/2, -1/6, 0, 0, 2/6, -2/5, 3/5, 1/6, -1/6, 0, 0, 1/6, 1/4];

var pitches3 = [5500, [5600, 5950], 5700, 6050];
var rhythm3 = [-1/8, 1/8, 1/8, 1/8];

pitches2.do({pitches1=pitches1++0});
pitches2.do({pitches3=pitches3++0});

o = SuperOM.new;
o.writeOMfile("example9.omi", [pitches1, pitches2, pitches3], rhythmTree: [rhythm1, rhythm2, rhythm3], metronome: 144);
)



//Interestingly, rhythm trees that contain 0 make subsequent notes to collapse, thus creating chords.
// See next example that compares two different ways of writing the same musical score.
// As before, also here we need to "zero pad" the shortest array.
(
var pitches1 = [6000, [6200, 6550, 6800], 7000, 6800, [5300, 5625, 6378], 6740];
var rhythm1 = [1/2, -1/6, 2/6, -2/5, 3/5, 1/6, -1/6, 1/6, 1/4];

var pitches2 = [6000, 6200, 6550, 6800, 7000, 6800, 5300, 5625, 6378, 6740];
var rhythm2 = [1/2, -1/6, 0, 0, 2/6, -2/5, 3/5, 1/6, -1/6, 0, 0, 1/6, 1/4];

pitches2.size.do({pitches1=pitches1++0});

o = SuperOM.new;
o.writeOMfile("example10.omi", [pitches1, pitches2], rhythmTree: [rhythm1, rhythm2], metronome: 144);
)



//EOF