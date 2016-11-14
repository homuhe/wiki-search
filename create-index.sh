#!/bin/bash

mvn scala:run -DmainClass=com.ir.IndexCreator -DaddArgs="$1|$2"
