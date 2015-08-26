#!/bin/bash

INCLUDES=$(python-config --includes)
LIBS=$(python-config --libs)
echo "cat _oasis.tmpl | sed s#PYTHON_INCLUDES#$INCLUDES $LIBS#g > _oasis"  
cat _oasis.tmpl |  
	sed "s#PYTHON_INCLUDES#$INCLUDES#g" |  
	sed "s#PYTHON_LIBS#$LIBS#g" > _oasis 
