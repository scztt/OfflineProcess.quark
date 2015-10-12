OfflineProcess {
	var <passes, <args;
	var outputFiles;
	var <blockSize=64;
	var <inputDuration, <inputChannels, <sampleRate, <controlRate;
	var <outputPath;
	var <resultFiles;

	*new {
		^super.new.init;
	}

	init {
		passes = IdentityDictionary();
		args = IdentityDictionary();
	}

	at {
		|name|
		^passes[name]
	}

	putAr {
		|name, func|
		var pass = OfflinePassAr(name, func);
		passes[name] = pass;
		^pass
	}

	putKr {
		|name, func|
		var pass = OfflinePassKr(name, func);
		passes[name] = pass;
		^pass
	}

	putTrig {
		|name, func|
		var pass = OfflinePassTrig(name, func);
		passes[name] = pass;
		^pass
	}

	outputPath_{
		|path|
		outputPath = path;
	}

	process {
		|file|
		var score, buffers, synths, defs;
		var preroll = 0, duration;
		var fileIteration = 0;
		var outPaths, inputFile;

		inputFile = SoundFile();
		if (inputFile.openRead(file).not) {
			Exception("Could not read file: %".format(file)).throw;
		} {
			sampleRate = inputFile.sampleRate;
			controlRate = (sampleRate / blockSize).floor;
			duration = inputDuration = inputFile.numFrames / inputFile.sampleRate;
			inputChannels = inputFile.numChannels;
			"INPUT FILE:".postln;
			"  sampleRate: %".format(sampleRate).postln;
			"  controlRate: %".format(controlRate).postln;
			"  duration: %".format(duration).postln;
			"  inputChannels: %".format(inputChannels).postln;

			passes.do {
				|pass|
				if (pass.preroll > preroll) {
					preroll = preroll;
				};
				if (pass.duration(inputDuration) > duration) {
					duration = pass.duration(inputDuration);
				}
			};

			outPaths = IdentityDictionary();
			passes.keysValuesDo {
				|name, pass|
				var path, filename;

				path = (outputPath ?? PathName.tmp);
				filename = "%_" ++ "%.aiff".format(name);
				while { File.exists(path +/+ filename.format(fileIteration)) } {
					fileIteration = fileIteration + 1;
				};

				outPaths[name] = path +/+ filename;
			};
			outPaths = outPaths.collect(_.format(fileIteration));

			defs = this.makeDefs();
			buffers = this.makeBuffers();
			synths = this.makeSynths(defs);

			score = Score();
			passes.keysValuesDo {
				|name, pass|

				buffers[name].do {
					|buffer|
					var path, filename;

					// create buffer
					score.add([0, buffer.allocMsg]);
					score.add([0, buffer.fillMsg(0, buffer.numFrames, pass.bufferFill)]);

					// write buffer to file
					score.add([
						duration + 0.1,
						buffer.writeMsg(
							outPaths[name].format(fileIteration), "aiff", "float"
						);
					])
				};

				synths[name].do {
					|synth|
					var startTime = preroll - pass.preroll;

					// start progress
					score.add([0,
						Synth.basicNew(\offlineProcessProgress, Server.default, 50).newMsg(
							args:[\duration, duration]
						)
					]);

					// start synth
					score.add([
						startTime,
						synth.newMsg(
							args:[\bufnum, buffers[name].bufnum] ++ args[name]
						)
					]);

					// free synth
					score.add([
						startTime + pass.duration(inputDuration),
						synth.freeMsg
					]);
				};
			};

			score.sort;
			score.score.do(_.postln);
			OSCdef(("%-%".format(this.class, this.identityHash)).asSymbol, {
				|msg|
				msg.postln;
			}, this.progressMsg);
			// score.recordNRT(
			// 	PathName.tmp +/+ this.identityHash.asString + ".osc",
			// 	"/dev/null",
			// 	file,
			// 	sampleRate, "aiff", "float",
			// 	options: Server.default.options.copy
			// 	.blockSize_(blockSize)
			// 	.numInputBusChannels_(inputChannels),
			// 	duration: duration + 1,
			// 	action: {
			// 		resultFiles = outPaths;
			// 		OSCdef(("%-%".format(this.class, this.identityHash)).asSymbol).free;
			// 	}
			// )
			^this.offlineProcess(
				score,
				PathName.tmp +/+ this.identityHash.asString + ".osc",
				"/dev/null",
				file,
				sampleRate, "aiff", "float",
				options: Server.default.options.copy
				.blockSize_(blockSize)
				.numInputBusChannels_(inputChannels),
				duration: duration + 1,
				action: {
					resultFiles = outPaths;
					OSCdef(("%-%".format(this.class, this.identityHash)).asSymbol).free;
				}
			)
		}
	}

	offlineProcess {
		|score, oscFilePath, outputFilePath, inputFilePath, sampleRate = 44100, headerFormat =
		"AIFF", sampleFormat = "int16", options, completionString="", duration = nil, action = nil|
		var cmd;

		if(oscFilePath.isNil) {
			oscFilePath = PathName.tmp +/+ "temp_oscscore" ++ UniqueID.next;
		};

		score.writeOSCFile(oscFilePath, 0, duration);
		cmd = (Score.program + " -N" + oscFilePath.quote
			+ if(inputFilePath.notNil, { inputFilePath.quote }, { "_" })
			+ outputFilePath.quote
		 	+ sampleRate + headerFormat + sampleFormat +
			(options ? Score.options).asOptionsString
			+ completionString);
		cmd.postln;
		^Pipe(cmd, "r");
	}

	makeBuffers {
		var buffers = IdentityDictionary();
		passes.keysValuesDo {
			|name, pass, i|
			buffers[name] = Buffer(
				Server.default,
				pass.numFrames(pass.duration(inputDuration), sampleRate, controlRate),
				pass.numChannels(inputChannels),
				i
			).postln;
		};
		^buffers;
	}

	progressMsg {
		^"/%/progress".format(this.identityHash).asSymbol
	}

	makeDefs {
		var defs = IdentityDictionary();

		SynthDef(\offlineProcessProgress, {
			|duration|
			var prog = Line.kr(0, 1, duration);
			SendReply.kr(prog % 0.01, this.progressMsg, prog);
		}).store;

		passes.keysValuesDo {
			|name, pass, i|
			var def = pass.makeDef(inputChannels.postln);
			def.store;
			defs[name] = def;
		};

		^defs
	}

	makeSynths {
		|defs|
		var synths = IdentityDictionary();
		passes.keysValuesDo {
			|name, pass, i|
			synths[name] = Synth.basicNew(
				defs[name].name,
				Server.default,
				1000 + i
			);
		};
		^synths
	}

	resultFile {
		|name|
		if (resultFiles.notNil) {
			^resultFiles[name]
		} {
			^nil
		}
	}

	resultData {
		|name|
		if (resultFiles.notNil) {
			^passes[name].handleData(resultFiles[name]);
		} {
			^nil
		}
	}
}

OfflinePass {
	var <name, <func, <>preroll, <>postroll;
	var outChannels;

	*new {
		|name, func, preroll=0, postroll=0|
		^super.newCopyArgs(name, func, preroll, postroll)
	}

	defName {
		^("%_%_%".format(this.class.name, name, func.def.sourceCode.hash)).asSymbol
	}

	makeDef {
		this.subclassResponsibility(\makeDef);
	}

	duration {
		|inputDuration|
		^(inputDuration + preroll + postroll)
	}

	numChannels {
		|inChannels|
		// outChannels comes from the synthdef structure itself, so build it if we don't have the value yet
		if (outChannels.isNil) {
			this.makeDef(inChannels);
		};

		^outChannels;
	}

	numFrames {
		this.subclassResponsibility(\bufferSamples);
	}

	bufferFill {
		^0
	}
}

OfflinePassAr : OfflinePass {
	numFrames {
		|inputDuration, sampleRate, controlRate|
		^(sampleRate * inputDuration)
	}

	makeDef {
		|inChannels, duration|
		inChannels.postln;
		^SynthDef(this.defName, {
			|bufnum|
			var progress;
			var time = Sweep.ar(1, 1);
			var sig = SoundIn.ar(Range(0, inChannels.postln).asArray);

			var output = SynthDef.wrap(func, prependArgs:[sig, time]);
			outChannels = output.isArray.if(output.size, 1);
			RecordBuf.ar(output, bufnum, loop:0);
		})
	}

	handleData {
		|file|
		var array, sf = SoundFile();
		if (sf.openRead(file)) {
			array = FloatArray.newClear(sf.numFrames * sf.numChannels);
			sf.readData(array);

			^array.clump(sf.numChannels).flop.collect({ |chan| Signal.newFrom(chan) })
		}
	}
}

OfflinePassKr : OfflinePass {
	numFrames {
		|inputDuration, sampleRate, controlRate|
		^(controlRate * inputDuration)
	}

	makeDef {
		|inChannels|
		^SynthDef(this.defName, {
			|bufnum|
			var time = Sweep.kr(1, 1);
			var sig = SoundIn.ar(Range(0, inChannels).asArray);

			var output = SynthDef.wrap(func, prependArgs:[sig, time]);
			outChannels = output.isArray.if(output.size, 1);

			RecordBuf.kr(output, bufnum, loop:0);
		})
	}

	handleData {
		|file|
		var array, sf = SoundFile();
		if (sf.openRead(file)) {
			array = FloatArray.newClear(sf.numFrames * sf.numChannels);
			sf.readData(array);

			^array.clump(sf.numChannels).flop.collect({ |chan| Signal.newFrom(chan) })
		}
	}
}

OfflinePassTrig : OfflinePass {
	var >bufferSize = 10000;

	numFrames {
		^bufferSize * this.numChannels;
	}

	makeDef {
		|inChannels|
		^SynthDef(this.defName, {
			|bufnum|
			var writers, writeIndex;
			var time = Sweep.kr(1, 1);
			var sig = SoundIn.ar(Range(0, inChannels).asArray);

			var trig, value;
			#trig, value = SynthDef.wrap(func, prependArgs:[sig, time]);
			outChannels = value.asArray.size;

			//Dbufwr(v, bufnum, Dseries(0, 1, bufferSize) + i)
			// Demand.kr(trig, 0, writers)
			BufWr.kr(value, bufnum, PulseCount.kr(trig), loop:0);
		})
	}

	handleData {
		|file|
		var array, result, fillValue, sf = SoundFile();
		if (sf.openRead(file)) {
			array = FloatArray.newClear(sf.numFrames);
			sf.readData(array);

			result = List(array.size);
			array.do {
				|v|
				if (v.isNaN.not) {
					result.add(v);
				}
			};
			^result.clump(sf.numChannels);
		}
	}

	bufferFill {
		^"nan".asFloat
	}
}
