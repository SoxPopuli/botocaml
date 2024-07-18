#!/usr/bin/env bash

create_lambda() {
    path=$1
    name=$(basename "${path}")
    dir=$(dirname "${path}")

    cd "${path}" || exit
    zip -0 -v "../${name}.zip" ./*

    cd - || exit

    awslocal lambda create-function \
        --function-name "$name" \
        --runtime nodejs20.x \
        --zip-file "fileb://${dir}/${name}.zip" \
        --handler "${name}.handler" \
        --role "arn:aws:iam::000000000000:role/service-role/${name}-role"
}

echo "poggers"

create_lambda "/src/lambda/simple"
