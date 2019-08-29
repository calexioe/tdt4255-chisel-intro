#!/bin/bash

COMPONENT=$1
BASE_FOLDER=$2

echo COMPONENT
echo BASE_FOLDER

rm ${BASE_FOLDER}/output_products/diagrams/${COMPONENT}/*

mkdir ${BASE_FOLDER}/output_products/diagrams/${COMPONENT}
cd /media/daniel/Data/Data/NTNU/TDT4255-Computer_Design/diagrammer
source /media/daniel/Data/Data/NTNU/TDT4255-Computer_Design/diagrammer/diagram.sh -i $BASE_FOLDER/output_products/fir/${COMPONENT}.fir -t $BASE_FOLDER/output_products/diagrams/${COMPONENT}
cd $BASE_FOLDER
