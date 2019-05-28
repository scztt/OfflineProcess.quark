OfflineProcessRun {
	var parentProcess, fileOrTime, <args;
	var outputFiles;
	var <blockSize=64;
	var <processingDeferred;
	var <>headerFormat="AIFF", <>sampleFormat="float";
	var <>inputDuration, <>inputChannels, <>sampleRate, <controlRate;
	var outputPath;
	var outPaths, inputFile;
	var <resultFiles;
	var <>preroll = 0, <>postroll = 0, <duration = 0.1;
	var score, buffers, synths, defs;
	var fileIteration = 0;
	var <resultString, <progress;

	*new {
		|parent, fileOrTime, args|
		^super.new.init(parent, fileOrTime, args)
	}

	init {
		|parent, inFileOrTime, inArgs|
		fileOrTime = inFileOrTime;
		parentProcess = parent;
		args = inArgs ?? { IdentityDictionary() };
		processingDeferred = Deferred();
		this.setup();
	}

	outputPath_{
		|path|
		outputPath = path;
	}

	outputPath {
		^outputPath ?? { parentProcess.outputPath }
	}

	progressMsg {
		^"/%/progress".format(this.identityHash).asSymbol
	}

	setup {
		var file;

		if (fileOrTime.isString) {
			inputFile = SoundFile();
			if (inputFile.openRead(fileOrTime).not) {
				Exception("Could not read file: %".format(fileOrTime)).throw;
			} {
				sampleRate = inputFile.sampleRate;
				controlRate = (sampleRate / blockSize).floor;
				duration = inputDuration = inputFile.numFrames / inputFile.sampleRate;
				inputChannels = inputFile.numChannels;
				// "INPUT FILE:".postln;
				// "  sampleRate: %".format(sampleRate).postln;
				// "  controlRate: %".format(controlRate).postln;
				// "  duration: %".format(duration).postln;
				// "  inputChannels: %".format(inputChannels).postln;
			}
		} {
			inputFile = nil;

			sampleRate = sampleRate ?? 48000;
			controlRate = (sampleRate / blockSize).floor;
			duration = inputDuration = fileOrTime ?? inputDuration ?? 30;
			inputChannels = inputChannels ?? 2;
			// "INPUT FILE:".postln;
			// "  sampleRate: %".format(sampleRate).postln;
			// "  controlRate: %".format(controlRate).postln;
			// "  duration: %".format(duration).postln;
			// "  inputChannels: %".format(inputChannels).postln;
		};

		outPaths = IdentityDictionary();
		parentProcess.passes.keysValuesDo {
			|name, pass|
			var path, filename;

			path = (outputPath ?? PathName.tmp);
			filename = "%_" ++ "%.%".format(name, headerFormat);
			while { File.exists(path +/+ filename.format(fileIteration)) } {
				fileIteration = fileIteration + 1;
			};

			outPaths[name] = path +/+ filename;
		};
		outPaths = outPaths.collect(_.format(fileIteration));
		outPaths.do {
			|p|
			File(p, "w").close();
		};

		Log(\OfflineProcess).debug("Output paths: " + outPaths.asString);

		defs = parentProcess.makeDefs(inputChannels, parentProcess.passes, this.progressMsg);
		synths = parentProcess.makeSynths(defs);
		buffers = this.makeBuffers();

		score = parentProcess.baseScore.deepCopy;
	}

	setup2 {

		parentProcess.passes.do {
			|pass|
			preroll = max(preroll, pass.preroll);
			postroll = max(postroll, pass.postroll);
			duration = max(duration, pass.duration(inputDuration));
		};

		parentProcess.additionalBuffers.do {
			|bufMsg|
			score.add([0, bufMsg]);
		};

		parentProcess.passes.keysValuesDo {
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
						outPaths[name].format(fileIteration), headerFormat, sampleFormat
					);
				])
			};

			synths[name].do {
				|synth|
				var startTime = preroll;

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
						args:[\bufnum, buffers[name].bufnum] ++ (args[name] !? _.asPairs ?? [])
					)
				]);

				// free synth
				score.add([
					startTime + this.duration(inputDuration),
					synth.freeMsg
				]);
			};
		};

		score.sort;
	}

	run {
		var proc;

		this.setup2();
		// score.recordNRT(
		// 	PathName.tmp +/+ this.identityHash.asString + ".osc",
		// 	"/dev/null",
		// 	inputFile.notNil.if(fileOrTime),
		// 	sampleRate, headerFormat, sampleFormat,
		// 	options: Server.default.options.copy
		// 	.blockSize_(blockSize)
		// 	.numInputBusChannels_(inputChannels)
		// 	.memSize_(2**20),
		// 	duration: duration + 1,
		// 	action: {
		// 		resultFiles = outPaths;
		// 		OSCdef(("%-%".format(this.class, this.identityHash)).asSymbol).free;
		// 		processingCondition.unhang;
		// 	}
		// )

		proc = this.offlineProcess(
			score,
			PathName.tmp +/+ this.identityHash.asString ++ ".osc",
			"/dev/null",
			inputFile.notNil.if(fileOrTime),
			sampleRate, "aiff", "float",
			options: Server.default.options.copy
				.blockSize_(blockSize)
				.numInputBusChannels_(inputChannels),
			duration: duration + 1,
			action: {
				|result|
			}
		);

		progress = 0;

		fork {
			var line;
			while { proc.isOpen } {
				line = proc.getLine();
				if (line.notNil) {
					line = line.findRegexp("__progress__: (.*)");
					if (line.size >= 2) {
						progress = line[1][1].asFloat;
						this.changed(\progress, progress);
					}
				} {
					proc.close();
				};
			};

			resultFiles = outPaths;
			progress = 1.0;
			processingDeferred.value = this;
		}
	}

	deferred {
		^processingDeferred
	}

	wait {
		processingDeferred.wait;
	}

	isDone {
		^processingDeferred.isResolved;
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

		Log(\OfflineProcess).debug(cmd);

		^Pipe(cmd, "r");
	}

	makeBuffers {
		var buffers = IdentityDictionary();
		parentProcess.passes.keysValuesDo {
			|name, pass, i|
			buffers[name] = Buffer(
				Server.default,
				pass.numFrames(pass.duration(inputDuration), sampleRate, controlRate),
				pass.numChannels(inputChannels),
				i
			);
		};
		^buffers;
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
			^parentProcess.passes[name].handleData(resultFiles[name]);
		} {
			^nil
		}
	}

}

OfflineProcess {
	var <passes, <args;
	var <additionalBuffers, <outputPath;
	var <>headerFormat="AIFF", <>sampleFormat="float";
	var <baseScore;

	*new {
		^super.new.init;
	}

	init {
		passes = IdentityDictionary();
		args = IdentityDictionary();
		baseScore = Score();
	}

	process {
		|fileOrTime|
		^OfflineProcessRun(this, fileOrTime).run();
	}

	at {
		|name|
		^passes[name]
	}

	putAr {
		|name, func|
		var pass = OfflinePassAr(name, func);
		passes[name] = pass;
	}

	putKr {
		|name, func|
		var pass = OfflinePassKr(name, func);
		passes[name] = pass;
	}

	putTrig {
		|name, func|
		var pass = OfflinePassTrig(name, func);
		passes[name] = pass;
	}

	outputPath_{
		|path|
		outputPath = path;
	}

	addBufferFromFile {
		|path|
		var buffer = Buffer(Server.default, 1, 1);
		additionalBuffers = additionalBuffers.add(buffer.allocReadMsg(path));
		^buffer
	}

	makeDefs {
		|inputChannels, passes, msg|
		var defs = IdentityDictionary();

		SynthDef(\offlineProcessProgress, {
			|duration|
			var prog = Line.kr(0, 1, duration);
			prog.poll(1, "__progress__");
		}).store;

		passes.keysValuesDo {
			|name, pass, i|
			var def = pass.makeDef(inputChannels);
			defs[name] = def;
		};

		^defs
	}

	makeSynths {
		|defs|
		var synths = IdentityDictionary();
		defs.keysValuesDo {
			|name, def, i|
			synths[name] = Synth.basicNew(
				def.name,
				Server.default,
				1000 + i
			);
		};
		^synths
	}
}

OfflinePass {
	var <name, <func, <>preroll, <>postroll;
	var outChannels, cachedDefs;

	*new {
		|name, func, preroll=0, postroll=0|
		^super.newCopyArgs(name, func, preroll, postroll).init
	}

	init {
		cachedDefs = ();
	}

	defName {
		^("%_%_%".format(this.class.name, name, func.def.sourceCode.hash)).asSymbol
	}

	makeDef {
		|inChannels, duration|
		if (cachedDefs[inChannels].isNil) {
			cachedDefs[inChannels] = this.prMakeDef(inChannels, duration);
			cachedDefs[inChannels].store();
		};
		^cachedDefs[inChannels]
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

	prMakeDef {
		|inChannels, duration|

		^SynthDef(this.defName, {
			|bufnum|
			var progress;
			var time = Sweep.ar(1, 1);
			var sig = SoundIn.ar(Range(0, inChannels).asArray);

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

	prMakeDef {
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
		} {
			Error("Error reading %".format(file)).throw;
		}
	}
}

OfflinePassTrig : OfflinePass {
	var >bufferSize = 10000;

	numFrames {
		^bufferSize * this.numChannels;
	}

	prMakeDef {
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
