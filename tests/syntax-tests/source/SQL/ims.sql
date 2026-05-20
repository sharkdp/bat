-- interships
create table interships 
(intership_id number(7) constraint intership_id_pk primary key,
name varchar2(50),
start_date date,
end_date date);


insert into interships
values
  (1,
   'Leaderator 2019',
   to_date('15/02/2019', 'DD/MM/YYYY'),
   to_date('01/09/2019', 'DD/MM/YYYY'));

insert into interships
  (intership_id, name, start_date)
values
  (2, 'Leaderator 2020', to_date('10/02/2019', 'DD/MM/YYYY'));

commit;

-- directions
create table directions 
(direction_id number(7) constraint direction_id_pk primary key,
name varchar2(50));

insert into directions values (1, 'Data Science');

insert into directions values (2, 'Oracle Development');

commit;

-- participants
create table participants 
(participant_id number(7) constraint participant_id_pk primary key,
first_name varchar2(25),
last_name varchar2(25),
personal_id number(11),
intership_id number(7) constraint participant_inter_id_fk references interships (intership_id),
direction_id number(7) constraint participant_direct_id_fk references directions (direction_id),
constraint personal_id_unique unique (personal_id));

insert into participants
values
  (1, 'Erekle', 'Tvinadze', 01011234567, 1, 1);

insert into participants
values
  (2, 'Mariami', 'Chakhvadze', 01011234568, 2, 2);

commit;

-- hiring_layer_types
create table hiring_layer_types 
(layer_type_id number(7) constraint layer_id_pk primary key,
type varchar2(50));

insert into hiring_layer_types values (1, 'GMAT');

insert into hiring_layer_types values (2, 'Algorithms');

insert into hiring_layer_types values (3, 'Interview');

commit;

-- hiring_layers
create table hiring_layers
(layer_id number(7) constraint layer_pk primary key,
layer_type_id number(7) constraint layer_type_fk references hiring_layer_types (layer_type_id),
participant_id number(7) constraint participant_id_fk references participants (participant_id),
result number(3));

insert into hiring_layers values (1, 1, 1, 52);

insert into hiring_layers values (2, 1, 2, 80);

insert into hiring_layers values (3, 2, 2, 75);

insert into hiring_layers values (4, 3, 2, 100);

commit;

-- subjects
create table subjects 
(subject_id number(7) constraint subject_id_pk primary key,
name varchar2(100),
minimum_score number(3));

insert into subjects values (1, 'SQL', 70);

insert into subjects values (2, 'Machine Learning', 70);

commit;

-- direction_subjects
create table direction_subjects
(direction_subject_id number(7) constraint direct_sub_id primary key,
direction_id number(7) constraint direct_id_fk references directions (direction_id),
subject_id number(7) constraint subject_id_fk references subjects (subject_id));

insert into direction_subjects values (1, 2, 1);

insert into direction_subjects values (2, 1, 1);

insert into direction_subjects values (3, 1, 2);

commit;

-- component_types
create table component_types
(component_type_id number(7) constraint com_type_id_pk primary key,
type varchar2(50));

insert into component_types values (1, 'Homework');

insert into component_types values (2, 'Quiz');

commit;

-- components
create table components
(component_id number(7) constraint component_id_pk primary key,
subject_id number(7) constraint sub_id_fk references subjects (subject_id),
minimum_score number(3),
component_type_id number(7) constraint com_type_id_fk references component_types (component_type_id),
weight number(3));

insert into components values (1, 1, 60, 1, 20);

insert into components values (2, 2, 65, 2, 50);

commit;

-- results
create table results 
(result_id number(7) constraint result_id_pk primary key,
issue_date date,
grade number(3),
participant_id number(7) constraint particip_id_fk references participants (participant_id),
component_id number(7) constraint component_id_fk references components (component_id));

insert into results
values
  (1, to_date('04/05/2020', 'DD/MM/YYYY'), 87, 2, 2);

commit;

-- learning_material_types
create table learning_material_types
(material_type_id number(7) constraint lear_material_id_pk primary key,
type varchar(50));

insert into learning_material_types values (1, 'Book');

insert into learning_material_types values (2, 'PPT');

insert into learning_material_types values (3, 'Youtube Video');

commit;

-- learning_materials
create table learning_materials
(learning_material_id number(7),
url varchar2(3000),
subject_id number(7) constraint subj_id_fk references subjects (subject_id),
material_type_id number(7) constraint material_type_id_fk references learning_material_types (material_type_id));

insert into learning_materials values (1, 'www.youtube.com', 1, 3);

commit;

-- sessions
create table sessions 
(session_id number(7) constraint session_id_pk primary key,
start_date date,
end_date date,
intership_id number(7) constraint inter_idd_fk references interships (intership_id),
direction_id number(7) constraint direct_fk references directions (direction_id),
subject_id number(7) constraint subject_fk references subjects (subject_id));

-- attendances
create table attendances
(attendance_id number(7) constraint attend_id_pk primary key,
participant_id number(7) constraint participant_fk references participants (participant_id),
session_id number(7) constraint session_id_fk references sessions (session_id),
status varchar2(25));


/*
შექმენით view სადაც იქნება სტაჟირების შესახებ ინფორმაცია:
სახელი,
დაწყების თარიღი,
დასრულების თარიღი,
მონაწილეების რაოდენობა.
*/
create view intership_info
as select i.name, i.start_date, i.end_date, (select count(participant_id) from participants p where p.intership_id = i.intership_id) number_of_participants
from interships i;

/*
შექმენით view სადაც იქნება მონაწილეებზე ინფორმაცია:
სტაჟირების სახელი,
მონაწილის სახელი,
მიმართულება,
შერჩევის რამდენი ეტაპი გაიარა,
სტაჟირების სტატუსი(გაიარა, ვერ გაიარა, მიმდინარე,ვერ მოხვდა სტაჟირებაზე)
*/
create view participant_info
as select i.name intership, p.first_name, p.last_name, d.name direction, 
(select count(h2.participant_id) from hiring_layers h2 where h2.participant_id = p.participant_id) number_of_layers,
case when p.direction_id is null then 'Rejected'
  when i.end_date is null then 'Present'
  when (select h2.result from hiring_layers h2 join hiring_layer_types l on h2.layer_type_id = l.layer_type_id where h2.participant_id = p.participant_id and l.type = 'Intership') > (select minimum_score from subjects) then 'Passed'
    else 'Failed'
      end  status
from interships i,
participants p,
directions d
where i.intership_id = p.intership_id
and (d.direction_id = p.direction_id or p.direction_id is null);

/*
შექნენით view სადაც იქნება მიმართულებებზე ინფორმაცია:
მიმართულების სახელი,
რა საგნები ისწავლება
*/
create view direction_info
as select d.name direction, s.name subject
from directions d,
direction_subjects ds,
subjects s
where d.direction_id = ds.direction_id
and ds.subject_id = s.subject_id;

/*
შექმენით view სადც იქნება საგნების ინფორმაცია:
საგნის სახელი,
საგნის ზღვარი,
შეფასების კომპონენტები(სახელი, ზღვარი , წონა).
*/
create view subject_info
as select s.name, s.minimum_score subject_min_score, ct.type, c.minimum_score component_min_score, c.weight
from subjects s,
component_types ct,
components c
where s.subject_id = c.subject_id 
and c.component_type_id = ct.component_type_id;

/*
შექმენით view დასწრების აღრიცხვა მონაწილეების მიხედვით:
სტაჟირებაზე მიმართულების მიხედვით:
რამდენი ჩატარდა,
რამდენს დაესწრო,
რამდენს არ დაესწრო.
*/
create view attendance_info
as select p.first_name, p.last_name, 
(select count(s2.session_id) from sessions s2 where s2.direction_id = p.direction_id) lectures,
(select count(a2.attendance_id) from attendances a2 where a2.participant_id = p.participant_id and a2.status = 'Present') present,
(select count(a2.attendance_id) from attendances a2 where a2.participant_id = p.participant_id and a2.status = 'Absent') absent
from participants p,
attendances a,
sessions s
where p.participant_id = a.participant_id
and a.session_id = s.session_id;
