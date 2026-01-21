create table main.ddic_database_table_element
(
    id            integer
        constraint ddic_database_table_element_pk
            primary key autoincrement,
    name          text not null,
    documentation text,
    is_key        integer,
    data_element  text not null,
    data_type     text,
    length        integer
);

create table main.ddic_structure_element
(
    id            integer,
    name          text not null,
    documentation text,
    is_key        integer,
    data_element  text,
    data_type     text not null,
    length        integer
);

create table main.sqlite_master
(
    type     TEXT,
    name     TEXT,
    tbl_name TEXT,
    rootpage INT,
    sql      TEXT
);

create table main.sqlite_sequence
(
    name,
    seq
);

create table main.src_file
(
    id        integer
        constraint src_file_pk
            primary key autoincrement,
    path      text not null,
    name      text not null,
    extension text not null,
    content   blob not null
);

create table main.dev_object
(
    id      integer
        constraint dev_object_pk
            primary key autoincrement,
    name    text    not null,
    kind    text    not null,
    file_id integer not null
        constraint dev_object_file_id_fk
            references main.src_file
);

create table main.ddic_data_element
(
    dev_object_id      integer not null
        constraint ddic_data_element_pk
            primary key
        constraint ddic_data_element_dev_object_id_fk
            references main.dev_object,
    name               text    not null,
    description        text,
    type_kind          text    not null,
    type_name          text    not null,
    data_type          text,
    data_type_length   integer,
    data_type_decimals integer
);

create table main.ddic_database_table
(
    dev_object_id integer not null
        constraint ddic_database_table_pk
            primary key
        constraint ddic_database_table_dev_object_id_fk
            references main.dev_object,
    name          text    not null,
    documentation text
);

create table main.ddic_database_table_to_elements
(
    table_id   integer not null
        constraint ddic_database_table_to_elements_ddic_database_table_dev_object_id_fk
            references main.ddic_database_table,
    element_id integer not null
        constraint ddic_database_table_to_elements_ddic_database_table_element_id_fk
            references main.ddic_database_table_element,
    constraint ddic_database_table_to_elements_pk
        primary key (table_id, element_id)
);

create table main.ddic_structure
(
    dev_object_id integer not null
        constraint ddic_structure_pk
            primary key
        constraint ddic_structure_dev_object_id_fk
            references main.dev_object,
    name          text    not null,
    documentation text
);

create table main.ddic_structure_to_elements
(
    dev_object_id integer not null
        constraint ddic_structure_to_elements_dev_object_id_fk
            references main.dev_object,
    element_id    integer not null
        constraint ddic_structure_to_elements_ddic_structure_element_id_fk
            references main.ddic_structure_element (id),
    constraint ddic_structure_to_elements_pk
        primary key (dev_object_id, element_id)
);

create table main.ddic_table_type
(
    dev_object_id integer not null
        constraint ddic_table_type_pk
            primary key
        constraint ddic_table_type_dev_object_id_fk
            references main.dev_object,
    name          text    not null,
    documentation text,
    element_type  text,
    is_row_type   integer
);

create table main.dev_object_to_dev_objects
(
    parent_id integer not null,
    child_id  integer not null,
    constraint dev_object_to_dev_objects_pk
        primary key (parent_id, child_id),
    constraint dev_object_to_dev_objects_dev_object_id_id_fk
        foreign key (parent_id, child_id) references main.dev_object (id, id)
);

