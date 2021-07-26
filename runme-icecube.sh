#!/bin/bash
sbt "runMain inferenceengine.iml.runtime.IMLInterpreterCmdLine --script before10temp2/scripts/ice-cube-example.iml --tablestoreIndex before10temp2/tablestore/tableindex.txt --outputPath output/ice-cube-example/ --twolineHeader true"
