#!bin/bash
function parse_file()
{
    echo "merge data"
    echo 'input file: '$1
    echo 'output file: '$2
    sed -i '1,500000d' $1
    sed -i '1000000, $d' $1
    python parse_jtl.py $1 $2
}
test_doc=`ls input`
count=`ls input|wc -w`
if [[ $count -eq 0 ]];
then
    echo "input should have origin jtl"
else
    for file in $test_doc
    do
        parse_file input/$file output/$file
    done
fi

#parse history jtl data to json data to ui, such as the following examples
python gen_jtl_data.py output jtl_json_test/test.json

