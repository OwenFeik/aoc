fs = require('fs');

var req_fields = [
    'byr',
    'iyr',
    'eyr',
    'hgt',
    'hcl',
    'ecl',
    'pid'
];

function part_one(data) {
    let count = 0;
    let current = [];
    data.split('\n').forEach(line => {
        if (line === '') {
            let valid = true; 
            req_fields.forEach(field => {
                if (!(current.indexOf(field) >= 0)) {
                    valid = false;
                }
            });

            if (valid) {
                count++;
            }
            current = [];
        }
        else {
            line.split(' ').forEach(token => {
                let [key, value] = token.split(':');
                current.push(key);
            });
        }
    });

    console.log(count);
}

function part_two(data) {
    let count = 0;
    let fields = 0;

    data.split('\n').forEach(line => {
        if (line === '') {
            if (fields === req_fields.length) {
                count++;
            }
            fields = 0;
        }
        else {
            line.split(' ').forEach(token => {
                let [key, value] = token.split(':');
    
                if (key === 'byr') {
                    yr = parseInt(value);
                    if (yr >= 1920 && yr <= 2002) {
                        fields++;
                    }
                }
                else if (key === 'iyr') {
                    yr = parseInt(value);
                    if (yr >= 2010 && yr <= 2020) {
                        fields++;
                    }
                }
                else if (key === 'eyr') {
                    yr = parseInt(value);
                    if (yr >= 2020 && yr <= 2030) {
                        fields++;
                    }
                }
                else if (key === 'hgt') {
                    unit = value.substr(-2);
                    size = parseInt(value.substr(0, value.length- 2));

                    if (unit === 'cm') {
                        if (size >= 150 && size <= 193) {
                            fields++;
                        }
                    }
                    else if (unit === 'in') {
                        if (size >= 59 && size <= 76) {
                            fields++;
                        }
                    }
                }
                else if (key === 'hcl') {
                    if (/^#[0-9a-f]{6}$/.test(value)) {
                        fields++;
                    }
                }
                else if (key === 'ecl') {
                    if ([
                        'amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth'
                    ].indexOf(value) >= 0) {
                        fields++;
                    }
                }
                else if (key === 'pid') {
                    if (/^[0-9]{9}$/.test(value)) {
                        fields++;
                    }
                }
            });
        }
    });
    console.log(count);
}

fs.readFile('in.txt', 'utf-8', (err, data) => {part_two(data)});    
