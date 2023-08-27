## 영화 데이터베이스 활용하기 
Use movie;

## 영화 및 TV 프로그램 목록 보기 
select * from netflixjson;


## 슈퍼장르 시청률 수 
select type,title, substring_index(substring_index(listed_in,',',1),',',-1) as genre 
from netflixjson ;

## 테이블에 '장르'추가
alter table netflixjson
add genre VARCHAR(255) AS (substring_index(substring_index(listed_in,',',1),',',-1));

#영화 슈퍼장르 시청률 수 
select genre, count(genre) as count from netflixjson
where type = 'Movie' group by genre 
order by count desc limit 10;

## TV쇼 슈퍼장르 시청률 수 
select genre, count(genre) as count from netflixjson
where type = 'TV Show' group by genre 
order by count desc limit 10;


## 세계 각국의 넷플릭스 시청자 수 
select country, count(*) as count from netflixjson
where country != "" 
group by country
order by count 
desc 
limit 10;


# 넷플릭스 영화와 TV쇼 시청 비율 
select type, count(type) as count from netflixjson group by type;


## 연도별 넷플릭스 영화 및 TV쇼 시청자 수
SELECT 
    release_year,
    COUNT(CASE WHEN type = 'Movie' THEN 1 END) AS movie,
    COUNT(CASE WHEN type = 'TV Show' THEN 1 END) AS tv
FROM netflixjson
GROUP BY release_year 
ORDER BY release_year;

## 영화 등급별 시청자 수 
SELECT rating, count(*) as count from netflixjson
GROUP BY rating 
HAVING rating IN ('G','PG','PG-13','R','NC-17', 'NR','UR')
ORDER BY count
DESC;

## 티비 등급별 시청자 수 
SELECT rating, count(*) as count from netflixjson
GROUP BY rating 
HAVING rating IN ('TV-Y7','TV-Y','TV-Y7','TV-G','TV-PG', 'TV-14','TV-MA')
ORDER BY count
DESC;
