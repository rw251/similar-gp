- For each read hierarchy X???? get count for each practice.
- Also get practice size and some demographics:



select p.practice, left(readcode,1), year(entrydate), count(*) from journal j inner join patients p on p.id = j.patid
group by p.practice, left(readcode,1), year(entrydate);


select gp, avg(age), count(*), avg(dep), sum(case when sex='M' then 1 else 0 end) as numMen from patients
group by gp;