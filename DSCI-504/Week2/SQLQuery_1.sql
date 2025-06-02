CREATE TABLE IF NOT EXISTS airline(
    PRIMARY KEY(airline_id)
    ,airline_id INTEGER NOT NULL
    ,airline_name VARCHAR(50)
    ,airline_abbrev VARCHAR(3)
    ,airline_hq_city VARCHAR(50)
    ,airline_hq_state CHAR(2)
    ,airline_num_emp INTEGER
    ,airline_motto TEXT
);