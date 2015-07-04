#!/bin/bash

sbt --error 'set showSuccess := false' "run src/main/resources/comet.hdsl" > ../hyperflow/examples/CometHdsl/workflow.json
