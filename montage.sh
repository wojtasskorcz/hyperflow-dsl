#!/bin/bash

# sbt --error 'set showSuccess := false' "run src/main/resources/montage01.hdsl" > ../hyperflow/data/0.1/workdir/dag_hdsl.json
sbt --error 'set showSuccess := false' "run src/main/resources/montage01v2.hdsl" > ../hyperflow/data/0.1/workdir/dag_hdslv2.json