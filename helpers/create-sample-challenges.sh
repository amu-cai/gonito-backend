#!/bin/bash -e

create-challenge()
{
    echo "================================="
    echo "CREATING challenge for $metric"

    metric="$1"

    name=${metric,,}

    dir=sample-challenge-${name}

    url="gitolite@gonito.net:${dir}"
    dont_peek_url="${url}-dont-peek"

    geval --init --expected-directory "$dir" --metric "$metric"

    cd $dir

    git init

    git remote add origin "$url"
    git remote add dont-peek "$dont_peek_url"

    git add config.txt dev-0 .gitignore README.md test-A/in.tsv train

    if [[ -r in-header.tsv ]]
    then
        git add in-header.tsv
    fi

    if [[ -r out-header.tsv ]]
    then
        git add out-header.tsv
    fi

    git commit -m 'Init'

    git checkout -b dont-peek

    git add test-A/expected.tsv

    git commit -m 'Hidden data'

    geval --validate --expected-directory .

    git push origin master
    git push dont-peek dont-peek

    git checkout master

    cd ..
}

create-submission ()
{
    submission_branch="$1"
    name="$2"
    val="$3"

    dir=sample-challenge-${name}
    url="gitolite@gonito.net:${dir}"

    cd "$dir"

    git checkout master

    git checkout -b $submission_branch

    for t in dev-0 test-A
    do
        while read i
        do
            echo "$val"
        done < $t/in.tsv > $t/out.tsv

        git add $t/out.tsv
    done

    git commit -m "$submission_branch"

    geval -t dev-0

    git push origin $submission_branch

    cd ..
}

create-challenge RMSE
create-challenge BLEU

create-submission sample-submission-1 rmse 10.0
create-submission sample-submission-2 rmse 15.0
create-submission sample-submission-1 bleu 'a ko te ahiahi'
