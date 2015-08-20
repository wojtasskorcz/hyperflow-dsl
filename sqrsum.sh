#!/bin/bash

sbt --error 'set showSuccess := false' "run src/main/resources/sqrsum.hdsl" > ../hyperflow/examples/SqrsumHdsl/workflow.json
