--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

SET search_path = public, pg_catalog;

ALTER TABLE ONLY public.survey_index DROP CONSTRAINT survey_index_case_study_id_fkey3;
ALTER TABLE ONLY public.survey_index DROP CONSTRAINT survey_index_case_study_id_fkey2;
ALTER TABLE ONLY public.survey_index DROP CONSTRAINT survey_index_case_study_id_fkey1;
ALTER TABLE ONLY public.survey_index DROP CONSTRAINT survey_index_case_study_id_fkey;
ALTER TABLE ONLY public.sampling_type DROP CONSTRAINT sampling_type_case_study_id_fkey;
ALTER TABLE ONLY public.sample DROP CONSTRAINT sample_vessel_id_fkey;
ALTER TABLE ONLY public.sample DROP CONSTRAINT sample_species_id_fkey;
ALTER TABLE ONLY public.sample DROP CONSTRAINT sample_sex_id_fkey;
ALTER TABLE ONLY public.sample DROP CONSTRAINT sample_maturity_stage_id_fkey;
ALTER TABLE ONLY public.sample DROP CONSTRAINT sample_institute_id_fkey;
ALTER TABLE ONLY public.sample DROP CONSTRAINT sample_gear_id_fkey;
ALTER TABLE ONLY public.sample DROP CONSTRAINT sample_case_study_id_fkey3;
ALTER TABLE ONLY public.sample DROP CONSTRAINT sample_case_study_id_fkey2;
ALTER TABLE ONLY public.sample DROP CONSTRAINT sample_case_study_id_fkey1;
ALTER TABLE ONLY public.sample DROP CONSTRAINT sample_case_study_id_fkey;
ALTER TABLE ONLY public.prey DROP CONSTRAINT prey_species_id_fkey;
ALTER TABLE ONLY public.prey DROP CONSTRAINT prey_predator_id_fkey;
ALTER TABLE ONLY public.prey DROP CONSTRAINT prey_digestion_stage_id_fkey;
ALTER TABLE ONLY public.predator DROP CONSTRAINT predator_vessel_id_fkey;
ALTER TABLE ONLY public.predator DROP CONSTRAINT predator_stomach_state_id_fkey;
ALTER TABLE ONLY public.predator DROP CONSTRAINT predator_species_id_fkey;
ALTER TABLE ONLY public.predator DROP CONSTRAINT predator_sex_id_fkey;
ALTER TABLE ONLY public.predator DROP CONSTRAINT predator_maturity_stage_id_fkey;
ALTER TABLE ONLY public.predator DROP CONSTRAINT predator_institute_id_fkey;
ALTER TABLE ONLY public.predator DROP CONSTRAINT predator_gear_id_fkey;
ALTER TABLE ONLY public.predator DROP CONSTRAINT predator_case_study_id_fkey3;
ALTER TABLE ONLY public.predator DROP CONSTRAINT predator_case_study_id_fkey2;
ALTER TABLE ONLY public.predator DROP CONSTRAINT predator_case_study_id_fkey1;
ALTER TABLE ONLY public.predator DROP CONSTRAINT predator_case_study_id_fkey;
ALTER TABLE ONLY public.index_type DROP CONSTRAINT index_type_case_study_id_fkey;
ALTER TABLE ONLY public.division DROP CONSTRAINT division_case_study_id_fkey1;
ALTER TABLE ONLY public.division DROP CONSTRAINT division_case_study_id_fkey;
ALTER TABLE ONLY public.data_source DROP CONSTRAINT data_source_case_study_id_fkey;
ALTER TABLE ONLY public.areacell DROP CONSTRAINT areacell_case_study_id_fkey;
ALTER TABLE ONLY public.vessel DROP CONSTRAINT vessel_pkey;
ALTER TABLE ONLY public.vessel DROP CONSTRAINT vessel_name_key;
ALTER TABLE ONLY public.survey_index DROP CONSTRAINT survey_index_pkey;
ALTER TABLE ONLY public.stomach_state DROP CONSTRAINT stomach_state_pkey;
ALTER TABLE ONLY public.stomach_state DROP CONSTRAINT stomach_state_name_key;
ALTER TABLE ONLY public.species DROP CONSTRAINT species_pkey;
ALTER TABLE ONLY public.species DROP CONSTRAINT species_name_key;
ALTER TABLE ONLY public.sex DROP CONSTRAINT sex_pkey;
ALTER TABLE ONLY public.sex DROP CONSTRAINT sex_name_key;
ALTER TABLE ONLY public.sampling_type DROP CONSTRAINT sampling_type_pkey;
ALTER TABLE ONLY public.sampling_type DROP CONSTRAINT sampling_type_case_study_id_name_key;
ALTER TABLE ONLY public.sample DROP CONSTRAINT sample_pkey;
ALTER TABLE ONLY public.prey DROP CONSTRAINT prey_pkey;
ALTER TABLE ONLY public.predator DROP CONSTRAINT predator_pkey;
ALTER TABLE ONLY public.maturity_stage DROP CONSTRAINT maturity_stage_pkey;
ALTER TABLE ONLY public.maturity_stage DROP CONSTRAINT maturity_stage_name_key;
ALTER TABLE ONLY public.market_category DROP CONSTRAINT market_category_pkey;
ALTER TABLE ONLY public.market_category DROP CONSTRAINT market_category_name_key;
ALTER TABLE ONLY public.institute DROP CONSTRAINT institute_pkey;
ALTER TABLE ONLY public.institute DROP CONSTRAINT institute_name_key;
ALTER TABLE ONLY public.index_type DROP CONSTRAINT index_type_pkey;
ALTER TABLE ONLY public.index_type DROP CONSTRAINT index_type_case_study_id_name_key;
ALTER TABLE ONLY public.gear DROP CONSTRAINT gear_pkey;
ALTER TABLE ONLY public.gear DROP CONSTRAINT gear_name_key;
ALTER TABLE ONLY public.fleet DROP CONSTRAINT fleet_pkey;
ALTER TABLE ONLY public.fleet DROP CONSTRAINT fleet_name_key;
ALTER TABLE ONLY public.division DROP CONSTRAINT division_pkey;
ALTER TABLE ONLY public.digestion_stage DROP CONSTRAINT digestion_stage_pkey;
ALTER TABLE ONLY public.digestion_stage DROP CONSTRAINT digestion_stage_name_key;
ALTER TABLE ONLY public.data_source DROP CONSTRAINT data_source_pkey;
ALTER TABLE ONLY public.data_source DROP CONSTRAINT data_source_case_study_id_name_key;
ALTER TABLE ONLY public.case_study DROP CONSTRAINT case_study_pkey;
ALTER TABLE ONLY public.case_study DROP CONSTRAINT case_study_name_key;
ALTER TABLE ONLY public.areacell DROP CONSTRAINT areacell_pkey;
ALTER TABLE ONLY public.areacell DROP CONSTRAINT areacell_case_study_id_name_key;
ALTER TABLE public.survey_index ALTER COLUMN survey_index_id DROP DEFAULT;
ALTER TABLE public.sample ALTER COLUMN sample_id DROP DEFAULT;
ALTER TABLE public.prey ALTER COLUMN prey_id DROP DEFAULT;
ALTER TABLE public.predator ALTER COLUMN predator_id DROP DEFAULT;
ALTER TABLE public.division ALTER COLUMN division_id DROP DEFAULT;
ALTER TABLE public.data_source ALTER COLUMN data_source_id DROP DEFAULT;
DROP TABLE public.vessel;
DROP SEQUENCE public.survey_index_survey_index_id_seq;
DROP TABLE public.survey_index;
DROP TABLE public.stomach_state;
DROP TABLE public.species;
DROP TABLE public.sex;
DROP TABLE public.sampling_type;
DROP SEQUENCE public.sample_sample_id_seq;
DROP TABLE public.sample;
DROP SEQUENCE public.prey_prey_id_seq;
DROP TABLE public.prey;
DROP SEQUENCE public.predator_predator_id_seq;
DROP TABLE public.predator;
DROP TABLE public.mfdb_schema;
DROP TABLE public.maturity_stage;
DROP TABLE public.market_category;
DROP TABLE public.institute;
DROP TABLE public.index_type;
DROP TABLE public.gear;
DROP TABLE public.fleet;
DROP SEQUENCE public.division_division_id_seq;
DROP TABLE public.division;
DROP TABLE public.digestion_stage;
DROP SEQUENCE public.data_source_data_source_id_seq;
DROP TABLE public.data_source;
DROP TABLE public.case_study;
DROP TABLE public.areacell;
DROP AGGREGATE public.weighted_stddev(numeric, numeric);
DROP AGGREGATE public.weighted_mean(numeric, numeric);
DROP FUNCTION public.weighted_stddev_final(p numeric[]);
DROP FUNCTION public.weighted_stddev_accum(p numeric[], n1 numeric, n2 numeric);
DROP FUNCTION public.weighted_mean_final(p numeric[]);
DROP FUNCTION public.weighted_mean_accum(p numeric[], n1 numeric, n2 numeric);
DROP EXTENSION plpgsql;
DROP SCHEMA public;
--
-- Name: public; Type: SCHEMA; Schema: -; Owner: lentinj
--

CREATE SCHEMA public;


ALTER SCHEMA public OWNER TO lentinj;

--
-- Name: SCHEMA public; Type: COMMENT; Schema: -; Owner: lentinj
--

COMMENT ON SCHEMA public IS 'standard public schema';


--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

--
-- Name: weighted_mean_accum(numeric[], numeric, numeric); Type: FUNCTION; Schema: public; Owner: lentinj
--

CREATE FUNCTION weighted_mean_accum(p numeric[], n1 numeric, n2 numeric) RETURNS numeric[]
    LANGUAGE sql IMMUTABLE
    AS $_$ SELECT ARRAY [
         $1[1] + $2 * $3,
         $1[2] + $3
       ] $_$;


ALTER FUNCTION public.weighted_mean_accum(p numeric[], n1 numeric, n2 numeric) OWNER TO lentinj;

--
-- Name: weighted_mean_final(numeric[]); Type: FUNCTION; Schema: public; Owner: lentinj
--

CREATE FUNCTION weighted_mean_final(p numeric[]) RETURNS numeric
    LANGUAGE sql IMMUTABLE
    AS $_$ SELECT $1[1] / $1[2] $_$;


ALTER FUNCTION public.weighted_mean_final(p numeric[]) OWNER TO lentinj;

--
-- Name: weighted_stddev_accum(numeric[], numeric, numeric); Type: FUNCTION; Schema: public; Owner: lentinj
--

CREATE FUNCTION weighted_stddev_accum(p numeric[], n1 numeric, n2 numeric) RETURNS numeric[]
    LANGUAGE sql IMMUTABLE
    AS $_$ SELECT ARRAY [
         $1[1] + $3,             -- total += weight
         $1[2] + $3 * $2,        -- sum += weight * value
         $1[3] + $3 * POW($2,2)  -- sqsum += weight * value**2
       ] $_$;


ALTER FUNCTION public.weighted_stddev_accum(p numeric[], n1 numeric, n2 numeric) OWNER TO lentinj;

--
-- Name: weighted_stddev_final(numeric[]); Type: FUNCTION; Schema: public; Owner: lentinj
--

CREATE FUNCTION weighted_stddev_final(p numeric[]) RETURNS double precision
    LANGUAGE sql IMMUTABLE
    AS $_$ SELECT CASE WHEN $1[1] < 2 THEN NULL ELSE |/ ( (1/($1[1] - 1)) * ($1[3] - POW($1[2], 2) / $1[1]) ) END $_$;


ALTER FUNCTION public.weighted_stddev_final(p numeric[]) OWNER TO lentinj;

--
-- Name: weighted_mean(numeric, numeric); Type: AGGREGATE; Schema: public; Owner: lentinj
--

CREATE AGGREGATE weighted_mean(numeric, numeric) (
    SFUNC = weighted_mean_accum,
    STYPE = numeric[],
    INITCOND = '{0,0}',
    FINALFUNC = weighted_mean_final
);


ALTER AGGREGATE public.weighted_mean(numeric, numeric) OWNER TO lentinj;

--
-- Name: weighted_stddev(numeric, numeric); Type: AGGREGATE; Schema: public; Owner: lentinj
--

CREATE AGGREGATE weighted_stddev(numeric, numeric) (
    SFUNC = weighted_stddev_accum,
    STYPE = numeric[],
    INITCOND = '{0,0,0,0}',
    FINALFUNC = weighted_stddev_final
);


ALTER AGGREGATE public.weighted_stddev(numeric, numeric) OWNER TO lentinj;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: areacell; Type: TABLE; Schema: public; Owner: lentinj; Tablespace: 
--

CREATE TABLE areacell (
    case_study_id integer NOT NULL,
    areacell_id integer NOT NULL,
    name character varying(1024) NOT NULL,
    size integer,
    CONSTRAINT areacell_name_check CHECK (((name)::text ~ '^[A-Za-z0-9_.\-]+$'::text))
);


ALTER TABLE public.areacell OWNER TO lentinj;

--
-- Name: COLUMN areacell.case_study_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN areacell.case_study_id IS 'Case study data is relevant to';


--
-- Name: COLUMN areacell.areacell_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN areacell.areacell_id IS 'Numeric ID for this entry';


--
-- Name: COLUMN areacell.name; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN areacell.name IS 'Short name used in data files / output data (in ltree notation)';


--
-- Name: COLUMN areacell.size; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN areacell.size IS 'Size of areacell';


--
-- Name: case_study; Type: TABLE; Schema: public; Owner: lentinj; Tablespace: 
--

CREATE TABLE case_study (
    case_study_id integer NOT NULL,
    name character varying(1024) NOT NULL,
    description character varying(1024),
    CONSTRAINT case_study_name_check CHECK (((name)::text ~ '^[A-Za-z0-9_.\-]+$'::text))
);


ALTER TABLE public.case_study OWNER TO lentinj;

--
-- Name: COLUMN case_study.case_study_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN case_study.case_study_id IS 'Numeric ID for this entry';


--
-- Name: COLUMN case_study.name; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN case_study.name IS 'Short name used in data files / output data (in ltree notation)';


--
-- Name: COLUMN case_study.description; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN case_study.description IS 'Long description';


--
-- Name: data_source; Type: TABLE; Schema: public; Owner: lentinj; Tablespace: 
--

CREATE TABLE data_source (
    case_study_id integer NOT NULL,
    data_source_id integer NOT NULL,
    name character varying(1024) NOT NULL,
    description character varying(1024),
    CONSTRAINT data_source_name_check CHECK (((name)::text ~ '^[A-Za-z0-9_.\-]+$'::text))
);


ALTER TABLE public.data_source OWNER TO lentinj;

--
-- Name: COLUMN data_source.case_study_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN data_source.case_study_id IS 'Case study data is relevant to';


--
-- Name: COLUMN data_source.data_source_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN data_source.data_source_id IS 'Numeric ID for this entry';


--
-- Name: COLUMN data_source.name; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN data_source.name IS 'Short name used in data files / output data (in ltree notation)';


--
-- Name: COLUMN data_source.description; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN data_source.description IS 'Long description';


--
-- Name: data_source_data_source_id_seq; Type: SEQUENCE; Schema: public; Owner: lentinj
--

CREATE SEQUENCE data_source_data_source_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.data_source_data_source_id_seq OWNER TO lentinj;

--
-- Name: data_source_data_source_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: lentinj
--

ALTER SEQUENCE data_source_data_source_id_seq OWNED BY data_source.data_source_id;


--
-- Name: digestion_stage; Type: TABLE; Schema: public; Owner: lentinj; Tablespace: 
--

CREATE TABLE digestion_stage (
    digestion_stage_id integer NOT NULL,
    name character varying(1024) NOT NULL,
    description character varying(1024),
    CONSTRAINT digestion_stage_name_check CHECK (((name)::text ~ '^[A-Za-z0-9_.\-]+$'::text))
);


ALTER TABLE public.digestion_stage OWNER TO lentinj;

--
-- Name: COLUMN digestion_stage.digestion_stage_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN digestion_stage.digestion_stage_id IS 'Numeric ID for this entry';


--
-- Name: COLUMN digestion_stage.name; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN digestion_stage.name IS 'Short name used in data files / output data (in ltree notation)';


--
-- Name: COLUMN digestion_stage.description; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN digestion_stage.description IS 'Long description';


--
-- Name: division; Type: TABLE; Schema: public; Owner: lentinj; Tablespace: 
--

CREATE TABLE division (
    division_id integer NOT NULL,
    case_study_id integer,
    division character varying(10) NOT NULL,
    areacell_id integer
);


ALTER TABLE public.division OWNER TO lentinj;

--
-- Name: TABLE division; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON TABLE division IS 'Grouping of area cells into divisions';


--
-- Name: COLUMN division.case_study_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN division.case_study_id IS 'Case study data is relevant to';


--
-- Name: COLUMN division.areacell_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN division.areacell_id IS 'Contained areacell';


--
-- Name: division_division_id_seq; Type: SEQUENCE; Schema: public; Owner: lentinj
--

CREATE SEQUENCE division_division_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.division_division_id_seq OWNER TO lentinj;

--
-- Name: division_division_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: lentinj
--

ALTER SEQUENCE division_division_id_seq OWNED BY division.division_id;


--
-- Name: fleet; Type: TABLE; Schema: public; Owner: lentinj; Tablespace: 
--

CREATE TABLE fleet (
    fleet_id integer NOT NULL,
    name character varying(1024) NOT NULL,
    description character varying(1024),
    CONSTRAINT fleet_name_check CHECK (((name)::text ~ '^[A-Za-z0-9_.\-]+$'::text))
);


ALTER TABLE public.fleet OWNER TO lentinj;

--
-- Name: COLUMN fleet.fleet_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN fleet.fleet_id IS 'Numeric ID for this entry';


--
-- Name: COLUMN fleet.name; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN fleet.name IS 'Short name used in data files / output data (in ltree notation)';


--
-- Name: COLUMN fleet.description; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN fleet.description IS 'Long description';


--
-- Name: gear; Type: TABLE; Schema: public; Owner: lentinj; Tablespace: 
--

CREATE TABLE gear (
    gear_id integer NOT NULL,
    name character varying(1024) NOT NULL,
    description character varying(1024),
    CONSTRAINT gear_name_check CHECK (((name)::text ~ '^[A-Za-z0-9_.\-]+$'::text))
);


ALTER TABLE public.gear OWNER TO lentinj;

--
-- Name: COLUMN gear.gear_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN gear.gear_id IS 'Numeric ID for this entry';


--
-- Name: COLUMN gear.name; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN gear.name IS 'Short name used in data files / output data (in ltree notation)';


--
-- Name: COLUMN gear.description; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN gear.description IS 'Long description';


--
-- Name: index_type; Type: TABLE; Schema: public; Owner: lentinj; Tablespace: 
--

CREATE TABLE index_type (
    case_study_id integer NOT NULL,
    index_type_id integer NOT NULL,
    name character varying(1024) NOT NULL,
    description character varying(1024),
    CONSTRAINT index_type_name_check CHECK (((name)::text ~ '^[A-Za-z0-9_.\-]+$'::text))
);


ALTER TABLE public.index_type OWNER TO lentinj;

--
-- Name: COLUMN index_type.case_study_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN index_type.case_study_id IS 'Case study data is relevant to';


--
-- Name: COLUMN index_type.index_type_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN index_type.index_type_id IS 'Numeric ID for this entry';


--
-- Name: COLUMN index_type.name; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN index_type.name IS 'Short name used in data files / output data (in ltree notation)';


--
-- Name: COLUMN index_type.description; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN index_type.description IS 'Long description';


--
-- Name: institute; Type: TABLE; Schema: public; Owner: lentinj; Tablespace: 
--

CREATE TABLE institute (
    institute_id integer NOT NULL,
    name character varying(1024) NOT NULL,
    description character varying(1024),
    CONSTRAINT institute_name_check CHECK (((name)::text ~ '^[A-Za-z0-9_.\-]+$'::text))
);


ALTER TABLE public.institute OWNER TO lentinj;

--
-- Name: COLUMN institute.institute_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN institute.institute_id IS 'Numeric ID for this entry';


--
-- Name: COLUMN institute.name; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN institute.name IS 'Short name used in data files / output data (in ltree notation)';


--
-- Name: COLUMN institute.description; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN institute.description IS 'Long description';


--
-- Name: market_category; Type: TABLE; Schema: public; Owner: lentinj; Tablespace: 
--

CREATE TABLE market_category (
    market_category_id integer NOT NULL,
    name character varying(1024) NOT NULL,
    description character varying(1024),
    CONSTRAINT market_category_name_check CHECK (((name)::text ~ '^[A-Za-z0-9_.\-]+$'::text))
);


ALTER TABLE public.market_category OWNER TO lentinj;

--
-- Name: COLUMN market_category.market_category_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN market_category.market_category_id IS 'Numeric ID for this entry';


--
-- Name: COLUMN market_category.name; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN market_category.name IS 'Short name used in data files / output data (in ltree notation)';


--
-- Name: COLUMN market_category.description; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN market_category.description IS 'Long description';


--
-- Name: maturity_stage; Type: TABLE; Schema: public; Owner: lentinj; Tablespace: 
--

CREATE TABLE maturity_stage (
    maturity_stage_id integer NOT NULL,
    name character varying(1024) NOT NULL,
    description character varying(1024),
    CONSTRAINT maturity_stage_name_check CHECK (((name)::text ~ '^[A-Za-z0-9_.\-]+$'::text))
);


ALTER TABLE public.maturity_stage OWNER TO lentinj;

--
-- Name: COLUMN maturity_stage.maturity_stage_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN maturity_stage.maturity_stage_id IS 'Numeric ID for this entry';


--
-- Name: COLUMN maturity_stage.name; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN maturity_stage.name IS 'Short name used in data files / output data (in ltree notation)';


--
-- Name: COLUMN maturity_stage.description; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN maturity_stage.description IS 'Long description';


--
-- Name: mfdb_schema; Type: TABLE; Schema: public; Owner: lentinj; Tablespace: 
--

CREATE TABLE mfdb_schema (
    version integer NOT NULL
);


ALTER TABLE public.mfdb_schema OWNER TO lentinj;

--
-- Name: TABLE mfdb_schema; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON TABLE mfdb_schema IS 'Table to keep track of schema version';


--
-- Name: COLUMN mfdb_schema.version; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN mfdb_schema.version IS 'Version of MFDB schema';


--
-- Name: predator; Type: TABLE; Schema: public; Owner: lentinj; Tablespace: 
--

CREATE TABLE predator (
    predator_id integer NOT NULL,
    data_source_id integer NOT NULL,
    case_study_id integer NOT NULL,
    institute_id integer,
    gear_id integer,
    vessel_id integer,
    sampling_type_id integer,
    year integer NOT NULL,
    month integer NOT NULL,
    areacell_id integer,
    stomach_name character varying(128) NOT NULL,
    species_id bigint,
    age integer,
    sex_id integer,
    maturity_stage_id integer,
    stomach_state_id integer,
    length real,
    weight real,
    CONSTRAINT predator_month_check CHECK (((month >= 1) AND (month <= 12)))
);


ALTER TABLE public.predator OWNER TO lentinj;

--
-- Name: TABLE predator; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON TABLE predator IS 'Predators in predator/prey sample';


--
-- Name: COLUMN predator.case_study_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN predator.case_study_id IS 'Case study data is relevant to';


--
-- Name: COLUMN predator.institute_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN predator.institute_id IS 'Institute that undertook survey';


--
-- Name: COLUMN predator.gear_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN predator.gear_id IS 'Gear used';


--
-- Name: COLUMN predator.vessel_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN predator.vessel_id IS 'Vessel used';


--
-- Name: COLUMN predator.sampling_type_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN predator.sampling_type_id IS 'Sampling type';


--
-- Name: COLUMN predator.year; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN predator.year IS 'Year sample was undertaken';


--
-- Name: COLUMN predator.month; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN predator.month IS 'Month sample was undertaken';


--
-- Name: COLUMN predator.areacell_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN predator.areacell_id IS 'Areacell data relates to';


--
-- Name: COLUMN predator.stomach_name; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN predator.stomach_name IS 'Stomach identifier';


--
-- Name: COLUMN predator.age; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN predator.age IS 'Age (years)';


--
-- Name: COLUMN predator.sex_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN predator.sex_id IS 'Sex ID';


--
-- Name: COLUMN predator.maturity_stage_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN predator.maturity_stage_id IS 'Maturity Stage ID';


--
-- Name: COLUMN predator.stomach_state_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN predator.stomach_state_id IS 'Status of stomach when caught';


--
-- Name: COLUMN predator.length; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN predator.length IS 'Length of predator';


--
-- Name: COLUMN predator.weight; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN predator.weight IS 'Weight of predator';


--
-- Name: predator_predator_id_seq; Type: SEQUENCE; Schema: public; Owner: lentinj
--

CREATE SEQUENCE predator_predator_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.predator_predator_id_seq OWNER TO lentinj;

--
-- Name: predator_predator_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: lentinj
--

ALTER SEQUENCE predator_predator_id_seq OWNED BY predator.predator_id;


--
-- Name: prey; Type: TABLE; Schema: public; Owner: lentinj; Tablespace: 
--

CREATE TABLE prey (
    prey_id integer NOT NULL,
    predator_id integer NOT NULL,
    species_id bigint,
    digestion_stage_id integer,
    length real,
    weight real,
    count integer DEFAULT 1 NOT NULL
);


ALTER TABLE public.prey OWNER TO lentinj;

--
-- Name: TABLE prey; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON TABLE prey IS 'Prey in predator/prey sample';


--
-- Name: COLUMN prey.predator_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN prey.predator_id IS 'The stomach this sample was found in';


--
-- Name: COLUMN prey.digestion_stage_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN prey.digestion_stage_id IS 'Digestion stage';


--
-- Name: COLUMN prey.length; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN prey.length IS 'Length of prey / mean length of all prey';


--
-- Name: COLUMN prey.weight; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN prey.weight IS 'Weight of prey / mean weight of all prey';


--
-- Name: COLUMN prey.count; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN prey.count IS 'Number of prey meeting this criteria';


--
-- Name: prey_prey_id_seq; Type: SEQUENCE; Schema: public; Owner: lentinj
--

CREATE SEQUENCE prey_prey_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.prey_prey_id_seq OWNER TO lentinj;

--
-- Name: prey_prey_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: lentinj
--

ALTER SEQUENCE prey_prey_id_seq OWNED BY prey.prey_id;


--
-- Name: sample; Type: TABLE; Schema: public; Owner: lentinj; Tablespace: 
--

CREATE TABLE sample (
    sample_id integer NOT NULL,
    data_source_id integer,
    case_study_id integer,
    institute_id integer,
    gear_id integer,
    vessel_id integer,
    sampling_type_id integer,
    year integer NOT NULL,
    month integer NOT NULL,
    areacell_id integer,
    species_id bigint,
    age integer,
    sex_id integer,
    maturity_stage_id integer,
    length real,
    length_var real,
    length_min integer,
    weight real,
    weight_var real,
    count integer DEFAULT 1 NOT NULL,
    CONSTRAINT sample_month_check CHECK (((month >= 1) AND (month <= 12)))
);


ALTER TABLE public.sample OWNER TO lentinj;

--
-- Name: TABLE sample; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON TABLE sample IS 'Samples within a survey';


--
-- Name: COLUMN sample.case_study_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN sample.case_study_id IS 'Case study data is relevant to';


--
-- Name: COLUMN sample.institute_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN sample.institute_id IS 'Institute that undertook survey';


--
-- Name: COLUMN sample.gear_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN sample.gear_id IS 'Gear used';


--
-- Name: COLUMN sample.vessel_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN sample.vessel_id IS 'Vessel used';


--
-- Name: COLUMN sample.sampling_type_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN sample.sampling_type_id IS 'Sampling type';


--
-- Name: COLUMN sample.year; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN sample.year IS 'Year sample was undertaken';


--
-- Name: COLUMN sample.month; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN sample.month IS 'Month sample was undertaken';


--
-- Name: COLUMN sample.areacell_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN sample.areacell_id IS 'Areacell data relates to';


--
-- Name: COLUMN sample.age; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN sample.age IS 'Age (years)';


--
-- Name: COLUMN sample.sex_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN sample.sex_id IS 'Sex ID';


--
-- Name: COLUMN sample.maturity_stage_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN sample.maturity_stage_id IS 'Maturity Stage ID';


--
-- Name: COLUMN sample.length; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN sample.length IS 'Length of fish / mean length of all fish';


--
-- Name: COLUMN sample.length_var; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN sample.length_var IS 'Length variance of all fish (given aggregated data)';


--
-- Name: COLUMN sample.length_min; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN sample.length_min IS 'Minimum theoretical value in this group';


--
-- Name: COLUMN sample.weight; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN sample.weight IS 'Weight of fish / mean weight of all fish';


--
-- Name: COLUMN sample.weight_var; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN sample.weight_var IS 'Weight variance of all fish (given aggregated data)';


--
-- Name: COLUMN sample.count; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN sample.count IS 'Number of fish meeting this criteria';


--
-- Name: sample_sample_id_seq; Type: SEQUENCE; Schema: public; Owner: lentinj
--

CREATE SEQUENCE sample_sample_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.sample_sample_id_seq OWNER TO lentinj;

--
-- Name: sample_sample_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: lentinj
--

ALTER SEQUENCE sample_sample_id_seq OWNED BY sample.sample_id;


--
-- Name: sampling_type; Type: TABLE; Schema: public; Owner: lentinj; Tablespace: 
--

CREATE TABLE sampling_type (
    case_study_id integer NOT NULL,
    sampling_type_id integer NOT NULL,
    name character varying(1024) NOT NULL,
    description character varying(1024),
    CONSTRAINT sampling_type_name_check CHECK (((name)::text ~ '^[A-Za-z0-9_.\-]+$'::text))
);


ALTER TABLE public.sampling_type OWNER TO lentinj;

--
-- Name: COLUMN sampling_type.case_study_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN sampling_type.case_study_id IS 'Case study data is relevant to';


--
-- Name: COLUMN sampling_type.sampling_type_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN sampling_type.sampling_type_id IS 'Numeric ID for this entry';


--
-- Name: COLUMN sampling_type.name; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN sampling_type.name IS 'Short name used in data files / output data (in ltree notation)';


--
-- Name: COLUMN sampling_type.description; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN sampling_type.description IS 'Long description';


--
-- Name: sex; Type: TABLE; Schema: public; Owner: lentinj; Tablespace: 
--

CREATE TABLE sex (
    sex_id integer NOT NULL,
    name character varying(1024) NOT NULL,
    description character varying(1024),
    CONSTRAINT sex_name_check CHECK (((name)::text ~ '^[A-Za-z0-9_.\-]+$'::text))
);


ALTER TABLE public.sex OWNER TO lentinj;

--
-- Name: COLUMN sex.sex_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN sex.sex_id IS 'Numeric ID for this entry';


--
-- Name: COLUMN sex.name; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN sex.name IS 'Short name used in data files / output data (in ltree notation)';


--
-- Name: COLUMN sex.description; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN sex.description IS 'Long description';


--
-- Name: species; Type: TABLE; Schema: public; Owner: lentinj; Tablespace: 
--

CREATE TABLE species (
    species_id bigint NOT NULL,
    name character varying(1024) NOT NULL,
    description character varying(1024),
    CONSTRAINT species_name_check CHECK (((name)::text ~ '^[A-Za-z0-9_.\-]+$'::text))
);


ALTER TABLE public.species OWNER TO lentinj;

--
-- Name: COLUMN species.species_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN species.species_id IS 'Numeric ID for this entry';


--
-- Name: COLUMN species.name; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN species.name IS 'Short name used in data files / output data (in ltree notation)';


--
-- Name: COLUMN species.description; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN species.description IS 'Long description';


--
-- Name: stomach_state; Type: TABLE; Schema: public; Owner: lentinj; Tablespace: 
--

CREATE TABLE stomach_state (
    stomach_state_id integer NOT NULL,
    name character varying(1024) NOT NULL,
    description character varying(1024),
    CONSTRAINT stomach_state_name_check CHECK (((name)::text ~ '^[A-Za-z0-9_.\-]+$'::text))
);


ALTER TABLE public.stomach_state OWNER TO lentinj;

--
-- Name: COLUMN stomach_state.stomach_state_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN stomach_state.stomach_state_id IS 'Numeric ID for this entry';


--
-- Name: COLUMN stomach_state.name; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN stomach_state.name IS 'Short name used in data files / output data (in ltree notation)';


--
-- Name: COLUMN stomach_state.description; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN stomach_state.description IS 'Long description';


--
-- Name: survey_index; Type: TABLE; Schema: public; Owner: lentinj; Tablespace: 
--

CREATE TABLE survey_index (
    survey_index_id integer NOT NULL,
    data_source_id integer,
    case_study_id integer,
    index_type_id integer,
    areacell_id integer,
    year integer NOT NULL,
    month integer NOT NULL,
    value real NOT NULL
);


ALTER TABLE public.survey_index OWNER TO lentinj;

--
-- Name: TABLE survey_index; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON TABLE survey_index IS 'Indices used to modify surveys';


--
-- Name: COLUMN survey_index.case_study_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN survey_index.case_study_id IS 'Case study data is relevant to';


--
-- Name: COLUMN survey_index.areacell_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN survey_index.areacell_id IS 'Areacell data relates to';


--
-- Name: COLUMN survey_index.year; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN survey_index.year IS 'Year sample was taken';


--
-- Name: COLUMN survey_index.month; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN survey_index.month IS 'Month sample was taken';


--
-- Name: COLUMN survey_index.value; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN survey_index.value IS 'Value at this point in time';


--
-- Name: survey_index_survey_index_id_seq; Type: SEQUENCE; Schema: public; Owner: lentinj
--

CREATE SEQUENCE survey_index_survey_index_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.survey_index_survey_index_id_seq OWNER TO lentinj;

--
-- Name: survey_index_survey_index_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: lentinj
--

ALTER SEQUENCE survey_index_survey_index_id_seq OWNED BY survey_index.survey_index_id;


--
-- Name: vessel; Type: TABLE; Schema: public; Owner: lentinj; Tablespace: 
--

CREATE TABLE vessel (
    vessel_id integer NOT NULL,
    name character varying(1024) NOT NULL,
    description character varying(1024),
    CONSTRAINT vessel_name_check CHECK (((name)::text ~ '^[A-Za-z0-9_.\-]+$'::text))
);


ALTER TABLE public.vessel OWNER TO lentinj;

--
-- Name: COLUMN vessel.vessel_id; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN vessel.vessel_id IS 'Numeric ID for this entry';


--
-- Name: COLUMN vessel.name; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN vessel.name IS 'Short name used in data files / output data (in ltree notation)';


--
-- Name: COLUMN vessel.description; Type: COMMENT; Schema: public; Owner: lentinj
--

COMMENT ON COLUMN vessel.description IS 'Long description';


--
-- Name: data_source_id; Type: DEFAULT; Schema: public; Owner: lentinj
--

ALTER TABLE ONLY data_source ALTER COLUMN data_source_id SET DEFAULT nextval('data_source_data_source_id_seq'::regclass);


--
-- Name: division_id; Type: DEFAULT; Schema: public; Owner: lentinj
--

ALTER TABLE ONLY division ALTER COLUMN division_id SET DEFAULT nextval('division_division_id_seq'::regclass);


--
-- Name: predator_id; Type: DEFAULT; Schema: public; Owner: lentinj
--

ALTER TABLE ONLY predator ALTER COLUMN predator_id SET DEFAULT nextval('predator_predator_id_seq'::regclass);


--
-- Name: prey_id; Type: DEFAULT; Schema: public; Owner: lentinj
--

ALTER TABLE ONLY prey ALTER COLUMN prey_id SET DEFAULT nextval('prey_prey_id_seq'::regclass);


--
-- Name: sample_id; Type: DEFAULT; Schema: public; Owner: lentinj
--

ALTER TABLE ONLY sample ALTER COLUMN sample_id SET DEFAULT nextval('sample_sample_id_seq'::regclass);


--
-- Name: survey_index_id; Type: DEFAULT; Schema: public; Owner: lentinj
--

ALTER TABLE ONLY survey_index ALTER COLUMN survey_index_id SET DEFAULT nextval('survey_index_survey_index_id_seq'::regclass);


--
-- Data for Name: areacell; Type: TABLE DATA; Schema: public; Owner: lentinj
--

COPY areacell (case_study_id, areacell_id, name, size) FROM stdin;
0	1	45G01	5
0	2	45G02	5
0	3	45G03	5
\.


--
-- Data for Name: case_study; Type: TABLE DATA; Schema: public; Owner: lentinj
--

COPY case_study (case_study_id, name, description) FROM stdin;
0	Test	For use in tests only
1	Baltic	Baltic Sea (SLU)
2	North	North Sea (NRC)
3	Iceland	Northern & Western Waters - Iceland Waters (MRI)
4	WScot	Northern Waters - West Scotland (UNIABDN)
5	Ibera	South-Western Waters - Iberian Waters (CSIC)
6	Medit	Mediterranean Waters - Strait of Sicily (CNR)
7	Black	Black Sea (INCDM)
\.


--
-- Data for Name: data_source; Type: TABLE DATA; Schema: public; Owner: lentinj
--

COPY data_source (case_study_id, data_source_id, name, description) FROM stdin;
0	1	cod2000	\N
0	2	survey1	\N
0	3	survey2	\N
\.


--
-- Name: data_source_data_source_id_seq; Type: SEQUENCE SET; Schema: public; Owner: lentinj
--

SELECT pg_catalog.setval('data_source_data_source_id_seq', 3, true);


--
-- Data for Name: digestion_stage; Type: TABLE DATA; Schema: public; Owner: lentinj
--

COPY digestion_stage (digestion_stage_id, name, description) FROM stdin;
1	1	Undigested
2	2	
3	3	
4	4	
5	5	Fully digested
\.


--
-- Data for Name: division; Type: TABLE DATA; Schema: public; Owner: lentinj
--

COPY division (division_id, case_study_id, division, areacell_id) FROM stdin;
7	0	divA	1
8	0	divA	2
9	0	divB	1
\.


--
-- Name: division_division_id_seq; Type: SEQUENCE SET; Schema: public; Owner: lentinj
--

SELECT pg_catalog.setval('division_division_id_seq', 9, true);


--
-- Data for Name: fleet; Type: TABLE DATA; Schema: public; Owner: lentinj
--

COPY fleet (fleet_id, name, description) FROM stdin;
\.


--
-- Data for Name: gear; Type: TABLE DATA; Schema: public; Owner: lentinj
--

COPY gear (gear_id, name, description) FROM stdin;
101	LLN	long line
102	GIL	gillnets
1021	GIL.GI1	gillnet, mesh 1''
1022	GIL.GI2	gillnet, mesh 2''
1023	GIL.GI3	gillnet, mesh 3''
1024	GIL.GI4	gillnet, mesh 4''
1025	GIL.GI5	gillnet, mesh 5''
1026	GIL.GI6	gillnet, mesh 6''
1027	GIL.GI7	gillnet, mesh 7''
1028	GIL.GI8	gillnet, mesh 8''
1029	GIL.GI9	gillnet, mesh 9''
10210	GIL.GI10	gillnet, mesh 10''
10211	GIL.GI11	gillnet, mesh 11''
10212	GIL.GI12	gillnet, mesh 12''
103	HLN	hand line
105	DSE	danish seine
106	BMT	bottom trawl
107	PGT	pelagic trawl
109	NPT	nephrops trawl
110	PSE	purse seine
114	SHT	shrimp trawl
115	DRD	dredge
117	TRP	trap
120	VAR	various gears
201	DP	demersal passive gear
2011	DP.DGN	demersal gill nets
202	PP	pelagic passive gear
2021	PP.GND	drift gillnets
2022	PP.PGN	pelagic gill nets
203	UP	undetermined passive gear
2031	UP.GEN	gillnets and entangling nets (not specified)
2032	UP.SAHL	small and hand lines
204	DA	demersal active gear
2041	DA.BT	beam trawl
2042	DA.DRB	boat dredge
2043	DA.OTB1	bottom otter trawl (side or stern not specified)
2044	DA.OTB2	bottom otter trawl (stern)
2045	DA.QST	queen scallop trawl
2046	DA.DPT	demersal pair trawl, demersal twin/mult trawl
2047	DA.DT	demersal trawl
2048	DA.PTB	bottom pair trawl - NB: operated by 2 vessels
2049	DA.SPR	pair seine, demersal pair seine - NB: operated by 2 vessels
20410	DA.SSC	scottish seine, demersal seine
20411	DA.TBN	nephrops trawl, nephrop trawl (single rig)
20412	DA.TBNT	nephrops trawl (twin/multiple rigged), twin/mult rig neph trawl
20413	DA.OTT	twin trawls, otter twin multi trawls
20414	DA.LT	light trawl (under 90ft)
205	PA	pelagic active gear
2051	PA.OT	otter trawl (not specified)
2052	PA.OTM	midwater otter trawl (side or stern not specified)
2053	PA.OTM1	midwater otter trawl (side)
2054	PA.OTM2	midwater otter trawl (stern)
2055	PA.PS1	pusre seine operated by one vessel
2056	PA.PS2	purse seine operated by two vessels
2057	PA.PS	purse seine
2058	PA.ST	shrimp trawl
2059	PA.SBPT	single boat pelagic trawl
20510	PA.IT	industrial trawl
20511	PA.PTP	pair trawl pelagic
20512	PA.PTM	midwater pair trawl - NB: operated by 2 vessels
20513	PA.TM	mid trawls (not specified)
206	UA	undetermined active gear
2061	UA.PT	pair trawl (not specified) - NB: operated by 2 vessels
2071	OD	other demersal nets
301	MBL	mobile
302	PAS	passive
303	POB	polyvalent bot
304	POM	polyvalent mobile
305	PAP	polyvalent passive
306	BTW	bottom trawl
307	DFN	dfn
308	DRG	dredge
309	DTDS	dtds
310	HKS	hooks
311	PTS	pts
312	POT	pot
\.


--
-- Data for Name: index_type; Type: TABLE DATA; Schema: public; Owner: lentinj
--

COPY index_type (case_study_id, index_type_id, name, description) FROM stdin;
0	99999	temperature	
\.


--
-- Data for Name: institute; Type: TABLE DATA; Schema: public; Owner: lentinj
--

COPY institute (institute_id, name, description) FROM stdin;
1	MRI	Marine Research Institute
2	IMR	Intitute of Marine Research
3	DIFRES	Danish Institute for Fisheries Research
4	SCUI	University of Iceland, Science Institute
5	UiB	University of Bergen
6	FRS	Fisheries Research Services
7	CEFAS	Centre for Environment, Fisheries and Aquaculture Science
8	IFREMER	Institut Francais de Recherche pour l'Exploitation de la Mer
9	RIVO	Netherlands Institute for Fisheries Research
10	ICES	International Council for the Exploration of the Sea
11	AZTI	AZTI Foundation
12	CSIC	Consejo Superior de Investigaciones Cientificas
13	IEO	Instituto Espanol de Oceanografia
14	NIMRD	 National Institute for Marine Research and Development
\.


--
-- Data for Name: market_category; Type: TABLE DATA; Schema: public; Owner: lentinj
--

COPY market_category (market_category_id, name, description) FROM stdin;
\.


--
-- Data for Name: maturity_stage; Type: TABLE DATA; Schema: public; Owner: lentinj
--

COPY maturity_stage (maturity_stage_id, name, description) FROM stdin;
1	1	Immature
2	2	
3	3	
4	4	
5	5	Mature
\.


--
-- Data for Name: mfdb_schema; Type: TABLE DATA; Schema: public; Owner: lentinj
--

COPY mfdb_schema (version) FROM stdin;
2
\.


--
-- Data for Name: predator; Type: TABLE DATA; Schema: public; Owner: lentinj
--

COPY predator (predator_id, data_source_id, case_study_id, institute_id, gear_id, vessel_id, sampling_type_id, year, month, areacell_id, stomach_name, species_id, age, sex_id, maturity_stage_id, stomach_state_id, length, weight) FROM stdin;
10	1	0	\N	\N	\N	\N	2000	1	2	AA	8791030402	\N	\N	\N	\N	21	210
11	1	0	\N	\N	\N	\N	2000	1	2	CC	8791030402	\N	\N	\N	\N	34	230
12	1	0	\N	\N	\N	\N	2000	1	2	BB	8791030402	\N	\N	\N	\N	34	220
\.


--
-- Name: predator_predator_id_seq; Type: SEQUENCE SET; Schema: public; Owner: lentinj
--

SELECT pg_catalog.setval('predator_predator_id_seq', 12, true);


--
-- Data for Name: prey; Type: TABLE DATA; Schema: public; Owner: lentinj
--

COPY prey (prey_id, predator_id, species_id, digestion_stage_id, length, weight, count) FROM stdin;
11	10	8755030201	1	1	10	5
\.


--
-- Name: prey_prey_id_seq; Type: SEQUENCE SET; Schema: public; Owner: lentinj
--

SELECT pg_catalog.setval('prey_prey_id_seq', 11, true);


--
-- Data for Name: sample; Type: TABLE DATA; Schema: public; Owner: lentinj
--

COPY sample (sample_id, data_source_id, case_study_id, institute_id, gear_id, vessel_id, sampling_type_id, year, month, areacell_id, species_id, age, sex_id, maturity_stage_id, length, length_var, length_min, weight, weight_var, count) FROM stdin;
13	2	0	1	102	1011	1	1998	1	1	8791030402	1	1	\N	10	\N	\N	100	\N	1
14	2	0	1	102	1011	1	1998	2	1	8791030402	2	2	\N	50	\N	\N	500	\N	1
15	2	0	1	102	1011	1	1998	3	1	8791030402	1	3	\N	30	\N	\N	300	\N	1
16	2	0	1	102	1011	1	1998	4	1	8791030402	2	1	\N	10	\N	\N	100	\N	1
17	2	0	1	102	1011	1	1998	5	1	8791030402	1	2	\N	35	\N	\N	350	\N	1
18	2	0	1	102	1011	1	1998	6	1	8791030402	2	3	\N	46	\N	\N	460	\N	1
19	2	0	1	102	1011	1	1998	7	1	8791030402	1	1	\N	65	\N	\N	650	\N	1
20	2	0	1	102	1011	1	1998	8	1	8791030402	2	2	\N	62	\N	\N	320	\N	1
21	2	0	1	102	1011	1	1998	9	1	8791030402	1	3	\N	36	\N	\N	360	\N	1
22	2	0	1	102	1011	1	1998	10	1	8791030402	2	1	\N	35	\N	\N	350	\N	1
23	2	0	1	102	1011	1	1998	11	1	8791030402	1	2	\N	34	\N	\N	340	\N	1
24	2	0	1	102	1011	1	1998	12	1	8791030402	2	3	\N	22	\N	\N	220	\N	1
25	3	0	10	105	1021	1	1998	1	1	8791031301	1	1	\N	35	\N	\N	110	\N	1
26	3	0	10	105	1021	1	1998	2	1	8791031301	2	2	\N	64	\N	\N	510	\N	1
27	3	0	10	105	1021	1	1998	3	1	8791031301	1	3	\N	23	\N	\N	310	\N	1
28	3	0	10	105	1021	1	1998	4	1	8791031301	2	1	\N	13	\N	\N	110	\N	1
29	3	0	10	105	1021	1	1998	5	1	8791031301	1	2	\N	99	\N	\N	310	\N	1
30	3	0	10	105	1021	1	1998	6	1	8791031301	2	3	\N	83	\N	\N	410	\N	1
31	3	0	10	105	1021	1	1998	7	1	8791031301	1	1	\N	54	\N	\N	610	\N	1
32	3	0	10	105	1021	1	1998	8	1	8791031301	2	2	\N	23	\N	\N	310	\N	1
33	3	0	10	105	1021	1	1998	9	1	8791031301	1	3	\N	65	\N	\N	310	\N	1
34	3	0	10	105	1021	1	1998	10	1	8791031301	2	1	\N	12	\N	\N	310	\N	1
35	3	0	10	105	1021	1	1998	11	1	8791031301	1	2	\N	22	\N	\N	310	\N	1
36	3	0	10	105	1021	1	1998	12	1	8791031301	2	3	\N	9	\N	\N	230	\N	1
\.


--
-- Name: sample_sample_id_seq; Type: SEQUENCE SET; Schema: public; Owner: lentinj
--

SELECT pg_catalog.setval('sample_sample_id_seq', 36, true);


--
-- Data for Name: sampling_type; Type: TABLE DATA; Schema: public; Owner: lentinj
--

COPY sampling_type (case_study_id, sampling_type_id, name, description) FROM stdin;
0	2	MOO	Seacow
0	1	SEA	Sea
\.


--
-- Data for Name: sex; Type: TABLE DATA; Schema: public; Owner: lentinj
--

COPY sex (sex_id, name, description) FROM stdin;
1	M	male
2	F	female
3	X	mixed
4	N	indeterminate
5	U	unknown
\.


--
-- Data for Name: species; Type: TABLE DATA; Schema: public; Owner: lentinj
--

COPY species (species_id, name, description) FROM stdin;
1000000000	LVE	Larvae (Fish Larvae)
1000000199	BSL	Shell (Broken Shell)
1501000000	SWB	Brown Seaweeds (Nei) (Phaeophyceae)
1508000000	KEL	Kelps-Tangles-Oarweeds (Phaeophyceae-Laminariales)
1508020000	LMY	Kelps Etc (Laminariaceae)
1508020100	LMX	Kelp (Laminaria Spp)
1508020104	LMS	Sugar Kelp (Laminaria Saccharina)
1508020110	LMT	Oarweed (Laminaria Digitata)
1510000000	WRK	Wracks (Phaeophyceae-Fucales)
1510010101	ANO	Knotted Wrack (Ascophyllum Nodosum)
1510010200	FUX	Wracks (Fucus Spp)
1510010201	WRB	Bladder Wrack (Fucus Vesiculosus)
1510010203	WRE	Flat Wrack (Fucus Spiralis)
1510010205	WRS	Toothed Wrack (Fucus Serratus)
1510010206	WRH	Horned Wrack (Fucus Ceranoides)
1510010302	WRC	Channeled Wrack (Pelvetia Canaliculata)
1601000000	SWR	Red Seaweeds (Nei) (Rhodophyceae)
1607070100	GEL	NA (Gelidium Spp)
1608100101	IMS	Irish Moss (Carragheen) (Chondrus Crispus)
1608100200	GIG	NA (Gigartina Spp)
1609070700	LIT	NA (Lithothamnion Spp)
1610020200	DUX	Dulse (Rhodymenia Spp)
1610020241	EDD	Sheep-Weed (Rhodymenia Palmata)
1611011500	PLZ	NA (Plumaria Spp)
2000000000	EGG	Eggs (Fish Eggs)
2000000199	ROK	Rocks (Assorted Rocks)
3000000199	BEN	Benthos (Epibenthic Mixture)
3600000000	PFZ	Sponges (Porifera)
3601000000	CCZ	Calcareous Sponges (Calcarea)
3607010000	LSZ	NA (Leucosoleniidae)
3607010100	LCX	NA (Leucosolenia Spp)
3608030000	GTZ	NA (Grantiidae)
3608030104	GAC	Purse Sponge (Grantia Compressa)
3665020000	HLZ	NA (Halichondridae)
3665020202	BCS	Breadcrumb Sponge (Halichondria Panicea)
3666030303	SUD	Sea Orange (Suberites Domuncula)
3667030100	TTA	NA (Tethya Spp)
3667030104	TAA	NA (Tethya Aurantia)
3668030100	GDA	NA (Geodia Spp)
3700000000	CAY	NA (Cnidaria)
3701000000	HZX	NA (Hydrozoa)
3702000000	HYD	Hydroids (Hydroida (Order))
3703010000	BGZ	NA (Bougainvilliidae)
3703010720	BGR	NA (Bougainvilla Ramosa)
3703030205	TBI	NA (Tubularia Indivisa)
3703030208	TBL	NA (Tubularia Larynx)
3703060102	CPU	NA (Coryne Pusilla)
3703080104	EDR	NA (Eudendrium Ramosum)
3704050316	WHW	White Weed (Sertularia Cupressina)
3704050507	THT	NA (Thuiara Thuja)
3717020101	PYH	NA (Physophora Hydrostatica)
3747020101	DMF	Dead-Mens Fingers (Alcyonium Digitatum)
3753060101	FAQ	NA (Funiculina Quadrangularis)
3754010202	VAM	NA (Virgularia Mirabilis)
3754020102	PNP	NA (Pennatula Phosphorea)
3756020104	EZI	NA (Epizoanthus Encrustans)
3760010403	DHA	Dahlia Anemone (Tealia Felina)
3760010701	BCT	NA (Bolocera Tuediae)
3760011008	AEQ	NA (Actinia Equina)
3760040201	HOD	NA (Hormathia Digitata)
3760040301	AMP	NA (Adamsia Palliata)
3760040403	ACR	NA (Actinauge Richardi)
3760060101	PMA	Plumose Anemone (Metridium Senile)
3768010102	DCC	Devonshire Cup-Coral (Caryophyllia Smithi)
3768014501	LPP	NA (Lophelia Pertusa)
3802010101	PBP	Sea Gooseberry (Pleurobrachia Pileus)
3802010201	BEC	NA (Beroe(Hormiphora) Cucumis)
3803010101	BOI	NA (Bolinopsis Infundibulum)
4302010107	TUA	Ribbon Worm (Tubulanus Annulatus)
4302030301	CXR	Ribbon Worm (Cephalothrix Rufifrons)
4303020212	CEF	Ribbon Worm (Cerebratulus Fuscus)
4303020402	RRW	Red Ribbon Worm (Lineus Ruber)
4306050120	AML	Ribbon Worm (Amphiphorus Lactifloreus)
4306060214	TTM	Ribbon Worm (Tetrastemma Melanocephalum)
4307010103	DSB	Flat Ribbon Worm (Drepanophorus Spectabilis)
5001000000	BWX	Bristle Worms (Polychaeta)
5001010105	AAC	Sea Mouse (Aphrodite Aculeata)
5001010204	HMH	NA (Laetmonice (Hermione) Histrix)
5001020807	HMI	NA (Harmothoe Impar)
5001021108	LNC	NA (Lepidonotus Clava)
5001021506	PYS	NA (Polynoe Scolopendrina)
5001130106	PZM	NA (Anaitides (Phyllodoce) Maculata)
5001131002	PZP	NA (Nereipyhlla (Phyllodoce) Paretti)
5001131408	PZL	NA (Phyllodoce Lamelligera)
5001200105	TOH	NA (Tomopteris Helgolandica)
5001230115	AUP	NA (Autolytus Pictus)
5001230512	SYF	NA (Typosyllis (Syllis) Prolifera)
5001240302	NEV	King Ragworm (Nereis(Neanthes)Virens)
5001240306	NEF	NA (Neanthes (Nereis) Fucata)
5001240400	NEX	Ragworms (Nereis Spp)
5001240403	NSA	Ragworm (Nereis Pelagica)
5001240411	NEE	Ragworm (Nereis Diversicolor)
5001241401	PCU	Ragworm (Perinereis Cultrifera)
5001250103	NEG	NA (Nephtys Caeca)
5001250112	NEH	NA (Nephtys Hombergii)
5001270110	GLC	NA (Glycera Convoluta)
5001290301	ONC	NA (Onuphis(Nothria)Conchylega)
5001290501	HYT	NA (Hyalinoecia Tubicola)
5001300115	ENH	NA (Eunice Harassii)
5001400301	SAJ	NA (Scoloplos Armiger)
5001400507	ORL	NA (Orbinia Latreillii)
5001430405	PDC	NA (Polydora Ciliata)
5001440108	MPP	NA (Magelona  Pappillicornis)
5001490101	CPV	NA (Chaetopterus Variopedatus)
5001500101	CCI	NA (Cirratulus Cirratus)
5001540202	FBA	NA (Flabelligera Affinis)
5001580303	OLB	NA (Ophelia Bicornis)
5001590101	SSC	NA (Sternapsis Scutata)
5001600101	CCP	NA (Capitella Capitata)
5001620204	LUG	Lug-Worm (Arenicola Marina)
5001630301	MDS	NA (Maldane Sarsi)
5001640102	OWF	NA (Owenia Fusiformis)
5001650200	RCL	Ross Coral (Sabellaria)
5001650206	SLA	NA (Sabellaria Alveolata)
5001660300	PEY	NA (Pectinaria Spp)
5001660305	PEK	NA (Pectinaria Koreni)
5001680104	AMJ	NA (Amphitrite Johnstoni)
5001682701	LCE	Sand Mason (Lanice Conchilega)
5001682901	AMG	NA (Amphitrides (Amphitrite) Gracilis)
5001700000	PWX	Peacock Worm (Sabellidae)
5001730401	SAV	NA (Serpula Vermicularis)
5001731001	FII	NA (Filograna Implexa)
5001731101	PRT	NA (Protula Tubularia)
5001731502	PST	NA (Pomatoceros Triqueter)
5001732301	DTA	NA (Ditrupa Arientina)
5001732701	MEE	NA (Mercierella Enigmatica)
5085000000	MOL	Molluscs (Mollusca)
5100000000	GAS	NA (Gastropoda)
5102000000	AGX	NA (G-Archaeogastropoda {Order})
5102030100	ABX	Ormers (Haliotis Spp)
5102030113	ORM	Ormer (Haliotis Tuberculata)
5102040404	LKY	Keyhole Limpet (Diodora (Fissurella) Apertura)
5102060000	LSX	Limpets (Patellidae)
5102060101	PPA	Blue-Rayed Limpet (Patina Pellucida (Heliion P))
5102060202	PTV	NA (Patella Vulgata)
5102060203	PDL	NA (Patella Depressa)
5102060204	LPA	NA (Patella Asprea (P. Athletica))
5102060205	PTI	NA (Patella Intermedia)
5102100000	TSX	Top Shells (Trochidae)
5102100111	PTS	Painted Top Shell (Calliostoma Ziziphinum)
5102100800	GTX	Top Shell (Gibbula Spp (Monodonta Spp))
5102100801	GTC	Grey Top Shell (Gibbula Cineraria)
5102100802	GTM	Top Shell (Gibbula Magus)
5102100803	GTU	Purple Top Shell (Gibbula Umbilicalis)
5102100804	GTT	Top Shell (Gibbula Tumida)
5102100806	GTP	Top Shell (Gibbula Pennanti)
5102101901	TPS	Top Shell (Cantharidus Exasperatus)
5102101902	GTS	Grooved Top Shell (Cantharidus Striatus)
5102102001	MDL	Toothed Winkle (Monodonta Lineata)
5102140102	PHS	Pheasant Shell (Tricolia (Phasyanella) Pullus)
5102250403	LWT	White Tortoiseshell (Acmaea(Tectura)Virginea)
5103000000	MGZ	NA (G-Mesogastropoda)
5103090303	LCR	NA (Lacuna Crassior)
5103090305	LCV	Banded Chink Shell (Lacuna Vincta)
5103090306	LAP	NA (Lacuna Pallidula)
5103090309	LCP	NA (Lacuna Parva)
5103100100	PER	Periwinkles (Nei) (Littorina Spp)
5103100105	LSP	Rough Winkle (Littorina Saxatilis)
5103100108	PEE	Edible Winkle (Littorina Littorea)
5103100109	LOP	Flat Winkle (Littorina Obtusata (L.Littoralis))
5103100122	LNP	Small Periwinkle (Littorina Neritoides)
5103130000	HYX	NA (Hydrobiidae)
5103130104	HYU	Laver Spire Shell (Hydrobia Ulvae (H.Stagnorum))
5103140000	TCX	NA (Truncatellidae)
5103140202	TCS	NA (Truncatella Subcylindrica)
5103200000	RIX	NA (Rissoidae)
5103200158	AVP	NA (Alvinia Punctura)
5103200159	AVL	NA (Alvinia Lactea)
5103200319	CIC	NA (Cingula Cingulus)
5103200320	CIT	NA (Cingula Semistriata)
5103200321	CIP	NA (Cingula Pulcherrima)
5103200322	CSC	NA (Cingula Semicostata)
5103200801	RIP	NA (Rissoa Parva)
5103200811	RII	NA (Rissoa Inconspicua)
5103200812	RIG	NA (Rissoa Guerini)
5103200816	RIM	NA (Zippora (Rissoa)  Membranacea)
5103210102	ASG	NA (Assiminea Grayana)
5103230000	TVE	NA (Vitrinellidae (Tornidae))
5103231505	TVG	NA (Tornus Greyana)
5103240102	SKO	NA (Skeneopsis Planorbis)
5103250201	AMR	NA (Ammonicera Rota)
5103270101	RID	NA (Rissoella Diaphana)
5103280101	CFA	NA (Cingulopsis (Rissoa) Fulgida)
5103330402	TUC	Tower Shell (Turritella Communis)
5103360302	CCG	NA (Caecum Glabrum)
5103460106	BTR	Needle Shell (Bittium Reticulatum)
5103500121	EPC	Wendle-Trap (Clathrus Clathrus)
5103500122	EPT	NA (Epitonium Turtonae)
5103500123	EPL	NA (Epitonium Clathratulum)
5103520301	GRA	NA (Graphis Albida)
5103530602	BAA	NA (Balcis Alba)
5103570102	APP	Pelicans Foot Shell (Aporrhais Pespelicani)
5103570103	ARS	NA (Aporrhais Serresiana)
5103630101	CAU	Bonnet Limpet (Capulus Ungaricus)
5103640103	CCS	Chinamans Hat Limpet (Calyptraea Chinenis)
5103640204	ASL	Slipper Limpet (Crepidula Fornicata)
5103660102	LMP	NA (Lamellaria Perspicua)
5103660104	LLL	NA (Lamellaria Latens)
5103660401	VEV	Velvet Shell (Velutina Velutina)
5103760206	NKS	Common Necklace Shell (Natica Alderi)
5103760212	NPP	NA (Natica (Polinices) Poliana)
5103760414	PNC	NA (Polinices Catena)
5103830203	TRA	European Cowrie (Trivia Arctica)
5103830204	TRM	European Cowrie (Trivia Monacha)
5103860211	BEU	NA (Bareeia Unifasciata)
5105010206	UXC	Oyster Drill (Urosalpinx Cinerea)
5105010207	OCE	Sting-Winkle(Drill) (Ocenebra Erinacea)
5105010505	DWK	Dog Whelk (Nucella (Thias) Lapillus)
5105011101	TPM	NA (Trophon Muricatus)
5105040000	WHZ	Whelks (Buccinidae)
5105040145	WHE	Common Whelk (Buccinum Undatum)
5105041101	CHB	NA (Chauvetea Brunnea)
5105050333	CSG	NA (Colus Gracilis)
5105050334	CSM	NA (Colus Marshalli)
5105050344	CSI	Iceland Whelk (Colus Islandicus)
5105050812	RWK	Red Whelk (Neptunea Antiqua)
5105080109	NAI	Thick-Lipped Dogwhelk (Nassarius Incrassatus)
5105080110	NSR	Netted Dog-Whelk (Nassarius Reticulatus)
5105080112	NAP	NA (Nassarius Pygmaeus)
5106020733	LTR	NA (Lora Turricula)
5108010142	ODP	NA (Odostomia Plicata)
5108010143	ODU	NA (Odostomia Unidentata)
5108010144	ONV	NA (Odostomia Nivosa)
5108010145	ODT	NA (Odostomia Truncatula)
5108010218	TUE	NA (Turbonilla Elegantissima)
5108010601	BYS	NA (Branchyostomia Scalaris)
5108010602	BRA	NA (Branchyostomia Albella)
5108010701	EUN	NA (Eulimella Nitidissima)
5108010801	CYO	NA (Chrysallida Obtusa)
5108010802	CYD	NA (Chrysallida Decussata)
5108010803	CYY	NA (Chrysallida Spiralis)
5110010103	ACT	NA (Acteon Tornatilis)
5110040304	SDL	NA (Scaphander Lignarius)
5110050109	PHP	NA (Philine Aperta)
5110050110	PHL	NA (Philine Angulata)
5110130106	REA	NA (Retusa Alba)
5113010106	LYR	NA (Limacina Retroversa)
5114040101	PHM	NA (Phytia Myosotis)
5114040401	LBD	NA (Leucophytia Bidentata)
5123030103	ELV	NA (Elysia Viridis)
5124000000	AAX	NA (Anaspidea)
5124020000	AYX	NA (Aplysiidae)
5124020206	AYP	Sea Hare (Aplysia Punctata)
5126020102	PBM	NA (Pleurobranchus Membranaceus)
5127000000	NBX	NA (Nudibranchia)
5128000000	NAD	NA (N-Doridoidea)
5130000000	NDX	NA (N-Doridoidea-Cryptobranchia)
5130020104	CAL	NA (Cadlina Laevis)
5130030304	ADP	Sea Lemon (Archidoris Pseudargus)
5131020102	PAD	NA (Palio Dubia)
5131020202	POQ	NA (Polycera Quadrilineata)
5131020801	LCL	NA (Limacia Clavigera)
5131050101	ACP	NA (Acanthodoris Pilosa)
5131050501	ODM	Fuzzy Onchidoris (Onchidoris(Lamellidoris)Muricata)
5131050510	ODQ	NA (Onchidoris(Lamellidoris)Pusilla)
5131050511	ODO	NA (Onchidoris (Lamellidoris) Oblongata)
5131060401	GDN	NA (Goniodoris Nodosa)
5131060501	OKA	NA (Okenia Aspersa)
5134010000	TNX	NA (Tritoniidae)
5134010100	TNZ	NA (Tritonia Spp)
5134010103	TNH	NA (Tritonia Hombergii)
5134010106	TNP	NA (Tritonia Plebeia)
5134010107	TNL	NA (Tritonia Lineata)
5134010108	TNO	NA (Tritonia Odhneri)
5134030101	LOM	NA (Lomanotus Marmoratus)
5134060103	DDF	NA (Dendronotus Frondosus)
5134090107	DOC	NA (Doto Cuspidata)
5136020104	AAL	NA (Armina Loveni)
5138020101	HOF	NA (Hero Formosa)
5140020000	CYZ	NA (Coyphellidae)
5141010100	EBX	NA (Eubranchus Spp)
5141010102	EBP	NA (Eubranchus Pallidus)
5141010103	EBR	NA (Eubranchus Tricola)
5141010107	EBE	NA (Eubranchus (Capellinia) Exiguus)
5141030201	CQA	NA (Catriona Aurantia)
5141030301	PQP	NA (Precuthona Peachi)
5141040100	CYX	NA (Coryphella Spp)
5141040110	CYV	NA (Coryphella Verrucosa)
5141040111	CYL	NA (Coryphella Lineata)
5141040112	CYQ	NA (Coryphella Pedata)
5141060101	CQG	NA (Calma Glaucoides)
5141070300	TQX	NA (Trinchesia Spp)
5141070301	TQC	NA (Trinchesia Caerulea)
5141070302	TQV	NA (Trinchesia Viridis)
5141070303	TQF	NA (Trinchesia Foliata)
5141070304	TQA	NA (Trinchesia Amoena)
5141070305	TQB	NA (Trinchesia Concinna)
5142010203	FAA	NA (Facelina Auriculata)
5142020301	FVB	NA (Favorinus Branchialis)
5142030101	AIP	Grey Sea Slug (Aeolidia Papillosa)
5142030401	ADA	NA (Aeolidiella Alderi)
5300000000	PLX	NA (Polyplacophora)
5302010000	LDX	NA (Lepidopleuridae)
5302010202	LDA	NA (Lepidopleurus Asellus)
5303020000	ISX	NA (Ischnochitonidae)
5303020302	ISA	NA (Ischnochiton Albus)
5303020403	LEC	NA (Lepidochiton Cinerea)
5303020604	TOR	NA (Tonicella Rubra)
5303040102	CNA	NA (Callochiton Achatinus)
5304010201	ACC	NA (Acanthochitona Crinata)
5304010206	ACO	NA (Acanthochiton Communis)
5500000000	BIV	NA (Mollusca-Bivalvia)
5502020000	NNX	NA (Nuculidae)
5502020212	NNU	Common Nut Shell (Nucula Nucleus)
5502020221	NTU	NA (Nucula Tumidula)
5506010400	ARK	Ark Shells (Arca Spp)
5506010403	ACL	NA (Arca Lactea)
5506060106	GLG	Dog Cockle (Glycymeris Glycymeris)
5507010000	MSX	Mussels (Nei) (Mytylidae (Mollusca))
5507010101	MUS	Blue(Edible)Mussel (Mytilus Edulis)
5507010103	MYG	Mediterranean Mussel (Mytilus Galloprovincialis)
5507010402	MDC	Green Crenella (Musculus Discors)
5507010407	MUM	Marbled Crenella (Musculus Marmoratus)
5507010414	MUC	NA (Musculus Costulatus)
5507010600	MOD	Horse Mussels (Modiolus Spp)
5507010601	HML	Horse Mussel (Modiolus Modiolus)
5507010605	BHM	Bean Horse Mussel (Modiolus Phaseolinus)
5507010606	BML	Bearded Horse Mussel (Modiolus Barbatus)
5509050000	SCX	Scallops (Pectinidae)
5509050100	CHY	Chlamys (Chlamys Sp)
5509050109	QSC	Queen Scallop (Chlamys Opercularis)
5509050110	CHV	Variegated Scallop (Chlamys Varia)
5509050111	CHD	Hunchback Scallop (Chlamys Distorta)
5509050112	CYT	Tiger Scallop (Chlamys Tigerina)
5509050114	CYS	Scallop (Chlamys Septemradiata)
5509050403	SCE	Escallop (Pecten Maximus)
5509090203	ASQ	Saddle Oyster (Heteroanemia (Anomia) Squamula)
5509090204	AEP	Common Saddle Oyster (Heteroanemia (Anomia) Ephippium)
5509090301	MSQ	NA (Monia Squama)
5509090302	MPF	Ribbed Saddle Oyster (Monia Patelliformis)
5509100000	LIZ	NA (Limidae)
5509100107	LIL	NA (Lima Loscombi)
5510020000	OYY	Oysters (Ostreidae)
5510020100	OYC	Cupped Oysters (Crassostrea Spp)
5510020101	OYG	Pacific Oyster (Crassostrea Gigas)
5510020102	OYA	American Oyster (Crassostrea Virginica)
5510020103	OYP	Portugese Oyster (Crassostrea Angulata)
5510020205	OYF	European Flat Oyster (Ostrea Edulis)
5515010203	PBH	NA (Lucinoma (Phacoidea) Borealis)
5515010402	MSA	NA (Myrtea Spinifera)
5515010703	DVD	NA (Divaricella Divaricata)
5515020300	TYX	NA (Thyasira Sp)
5515070102	LSR	NA (Lasaea Rubra)
5515080102	KSO	NA (Kellia Suborbicularis)
5515100108	MAB	NA (Mysella Bidentata)
5515100404	MCF	NA (Montacuta Ferruginosa)
5515190114	AEE	NA (Astarte Elliptica)
5515190116	AES	NA (Astarte Sulcata)
5515190120	AET	NA (Astarte Triangularis)
5515190204	AEB	NA (Astarte (Tridonta) Borealis)
5515190206	AEM	NA (Astarte (Tridonta) Montagui)
5515220000	COZ	Cockles  (Nei) (Cardiidae)
5515220404	LCC	NA (Laevicardium Crassum)
5515220602	COC	Common Cockle (Cerastoderma (Cardium) Edule)
5515221101	CSP	Cockle (Cardium (Parvicardium) Scabrum)
5515221103	CDE	Little Cockle (Cardium Exiguum)
5515221104	CPY	Prickly Cockle (Cardium (Acanthocardia) Echinata)
5515221106	SPC	Spiny Cockle (Cardium (Acanthocardia) Aculeata)
5515221200	ACY	NA (Acanthocardia Spp)
5515250000	MAZ	Trough Shells (Mactridae)
5515250105	ETS	Elliptical Trough Shell (Spisula Elliptica)
5515250106	TTS	Thick Trough Shell (Spisula Solida)
5515250107	CTH	Cut Trough Shell (Spisula Subtruncata)
5515250602	RTS	Rayed Trough Shell (Mactra Corallina)
5515250801	LUL	Common Otter Shell (Lutraria Lutraria)
5515250802	OTS	Otter Shell (Lutraria Magna)
5515290000	RAY	Razor Clams (Solenidae)
5515290200	RAZ	Razor Clams (Solen Spp)
5515290203	GRZ	Grooved Razor Shell (Solen Marginatus)
5515290300	ESY	NA (Ensis Spp)
5515290303	ESE	Razor Shell (Ensis Ensis)
5515290304	ESA	Razor Shell (Ensis Arcuatus)
5515290305	ESS	Pod Razor Shell (Ensis Siliqua)
5515300000	CEZ	NA (Cultellidae)
5515300101	CUP	NA (Cultellus Pellucidus)
5515310000	TEX	NA (Tellinidae)
5515310116	MCB	Baltic Tellin (Macoma Balthica)
5515310200	TAX	NA (Tellina Spp)
5515310219	TNT	Thin Tellin (Tellina Tenuis)
5515310220	TMD	NA (Tellina (Moerella) Donacina)
5515310221	TFF	NA (Tellina (Fabulina) Fabula)
5515310229	TCR	Blunt Tellin (Tellina (Arcopagia) Crassa)
5515310272	TPA	NA (Tellina (Moerella) Pygmaea)
5515320105	DOV	Banded Wedge Shell (Donax Vittatus)
5515330103	GIF	Faroe Sunset-Shell (Gari Fervensis)
5515350203	APM	NA (Abra Prismatica)
5515350204	ABA	NA (Abra Alba)
5515350205	ABT	NA (Abra Tenuis)
5515350401	PFS	Peppery Furrow Shell (Scrobularia Plana)
5515390101	CLQ	Icelandic Cyprina (Arctica Islandica)
5515470000	CLV	Carpet Shells (Veneridae)
5515470802	CTS	Pullet Carpet-Shell (Tapes (Venerupis) Pullastra)
5515470803	TVA	Golden Carpet-Shell (Tapes (Venerupis) Aureus)
5515470804	TVR	Banded Carpet-Shell (Tapes (Venerupis) Rhomboides)
5515470805	CTG	Grooved Carpetshell (Tapes (Venerupis) Decussata)
5515470806	TSD	NA (Tapes (Venerupis) Semidecussata)
5515470903	DSE	Rayed Artemis (Dosinia Exoleta)
5515470904	DSL	Smooth Artemis (Dosinia Lupinus)
5515471101	CLH	Hard-Shell Clam (Mercenaria (Venus) Mercenaria)
5515471602	GOM	NA (Gouldia Minima)
5515471633	GGS	NA (Gouldia(Gafrarium)Spinifera)
5515472101	VST	Striped Venus (Venus (Chamelea) Striatula)
5515472102	VEO	Oval Venus (Venus (Mercenaria) Ovata)
5515472104	VVR	Warty Venus (Venus (Mercenaria) Verrucosa)
5515472601	GGM	NA (Gafrarium Minimum)
5515480102	PPH	American Piddock (Petricola Pholadiformis)
5517010000	CLZ	Soft Shell Clams (Myidae)
5517010200	MYA	Mya (Mya Sp)
5517010201	CLS	Soft Clam/Sand-Gaper (Mya Arenaria)
5517010203	MYT	Blunt Gaper (Mya Truncata)
5517010401	SBI	NA (Sphenia Binghami)
5517020302	CGB	Common Basket Shell (Varicorbula Gibba)
5517060000	HSX	Rock Borers (Hiatellidae)
5517060201	HSA	NA (Hiatella Arctica)
5517060204	HSS	NA (Hiatella Striata)
5517060501	SXJ	NA (Saxicavella Jeffreysi)
5518010102	ZFC	Oval Piddock (Zirfaea Crispata)
5518010401	BNC	White Piddock (Barnea Candida)
5518010403	BAP	Little Piddock (Barnea Parva)
5518010902	CPK	Common Piddock (Pholas Dactylus)
5518020201	TRN	Shipworm (Teredo Navalis)
5518020202	TMG	Shipworm (Teredo Megotara)
5518020203	TDN	Shipworm (Teredo Norvagicus)
5520080200	THX	NA (Thracia Sp)
5520080212	TPH	NA (Thracia Phaseolina)
5600000000	SPZ	NA (Scaphopoda)
5601010107	DEV	NA (Dentalium Vulgare)
5601010201	DEE	NA (Dentalium Entale)
5700000000	CPZ	Squids Octopusses Etc. (Cephalopoda)
5704000000	CTL	Cuttle-Fishes (Cephalopoda-Sepiida)
5704020000	SPY	NA (Sepiolidae)
5704020104	ROM	NA (Rossia Macrosoma)
5704020301	SPA	Little Cuttle (Sepiola Atlantica)
5704020401	STO	NA (Sepietta Oweniana)
5704030000	SEY	NA (Sepiidae)
5704030101	SEE	Cuttle-Fish (Sepia Elegans)
5704030102	CTC	Common Cuttlefish (Sepia Officinalis)
5704030103	SEO	NA (Sepia Orbignyana)
5706010000	SQZ	Squids (Nei) (Loliginidae)
5706010100	SQC	Common Squids (Loligo Spp)
5706010104	NSQ	Northern Squid (Loligo Forbesi)
5706010105	LLV	Squid (Loligo Vulgaris)
5706010401	ATS	NA (Alloteuthis Subulata)
5707150000	OMX	NA (Ommastrephidae)
5707150301	SQI	Northern Shortfin Squid (Illex (Loligo) Illecebrosus)
5707150401	SQE	Flying Squid (Ommastrephes(Todarodes) Saggittatus)
5707150405	OME	NA (Ommastrephes (Todaropsis) Eblanae)
5707150406	OMT	NA (Ommastrephes Todarus)
5708010000	OCT	Octopuses (Octopodidae)
5708010202	OCV	Octopus (Octopus Vulgaris)
5708010501	EDC	Curled Octopus (Eledone Cirrosa)
5790000000	ARO	NA (Arthropoda (Phylum))
6001010000	NYZ	Sea Spiders (Nymphonidae)
6001010101	NNG	Sea Spider (Nymphon Grossipes)
6001010110	NYG	Sea Spider (Nymphon Gracile)
6001010111	NYH	Sea Spider (Nymphon Hirtum)
6001010112	NYT	Sea Spider (Nymphon Stroemi)
6001040207	ACE	Sea Spider (Achelia Echinata)
6001060102	PHF	Sea Spider (Phoxichilidium Femoratum)
6001060203	AOP	Sea Spider (Anoplodactylus Pygmaeus)
6001060206	ANP	NA (Anoplodactylus Petiolatus)
6001070101	EDS	Sea Spider (Endeis Spinosa)
6001080103	PGL	Sea Spider (Pycnogonum Littorale)
6001090201	CAB	NA (Callipallene Brevirostris)
6100000000	CRU	Marine Crustaceans (Crustacea)
6117000000	COP	Copepod Spp (Copepoda)
6120090123	OOA	NA (Oithona Atlantica)
6130000000	CIZ	Barnacles (Cirrepedia)
6132050000	GOZ	Goose Barnacles (Lepadidae)
6132050300	GOO	Goose Barnacles (Lepas Spp)
6132050301	GEB	Goose Barnacle (Lepas Antifera)
6134010000	TMX	NA (Chthamalidae)
6134020000	BEY	NA (Balanidae)
6145010000	NEZ	NA (Nebaliidae)
6145010101	NBB	NA (Nebalia Bipes)
6153010000	MIZ	NA (Mysidae)
6153011400	MYZ	Mysid Shrimps (Mysis Spp)
6154000000	CUZ	NA (P-Cumacea)
6154010000	LAZ	NA (Lampropidae)
6154010103	LPF	NA (Lamprops Fasciata)
6154020000	CMY	NA (Cumidae)
6154040000	LEZ	NA (Leuconidae)
6154040101	LEN	NA (Leucon Nasica)
6154050000	DYZ	NA (Diastylidae)
6154050116	DYR	NA (Diastylis Rathkei)
6154060000	PDZ	NA (Pseudocumidae)
6154060201	PSL	NA (Pseudocuma Longicornis)
6154090000	BOZ	NA (Bodotriidae)
6157010000	TAZ	NA (Tanaidae)
6159010000	GNZ	NA (Gnathiidae)
6160011700	ANY	NA (Anthura Spp)
6161010114	CIB	NA (Cirolana Borealis)
6161010204	EDP	NA (Eurydice Pulchra)
6161020700	SAY	NA (Sphaeroma Spp)
6161050100	LMZ	NA (Limnoria Spp)
6162010000	ARZ	NA (Arcturidae)
6162010402	ALC	NA (Astacilla Longicornis)
6162020000	IOZ	NA (Idoteidae)
6162020315	IAN	NA (Idotea Neglecta)
6162020317	IAG	NA (Idotea Granulosa)
6162020318	IAP	NA (Idotea Pelagica)
6163060000	JAZ	NA (Janiridae)
6163060200	JEX	NA (Jaera Spp)
6166010000	LGZ	NA (Ligiidae)
6166010100	LIY	NA (Ligia Spp)
6168000000	AAZ	NA (Amphipoda)
6169000000	GMZ	NA (Gammaridea)
6169010000	ACZ	NA (Acanthonotozomatidae)
6169010302	PPE	NA (Panoplea Eblanae)
6169020000	ALZ	NA (Ampeliscidae)
6169020100	ALY	NA (Ampelisca Spp)
6169020202	BYG	NA (Byblis Gaimardi)
6169020301	HAT	NA (Haploops Tubicola)
6169030000	ADZ	NA (Amphilochidae)
6169030200	AHY	NA (Amphilochus Spp)
6169040000	AEY	NA (Ampithoidae)
6169040100	AEX	NA (Ampithoe  Spp)
6169070000	ARX	NA (Argissidae)
6169070101	AGH	NA (Argissa Hamatipes)
6169090000	AYZ	NA (Atylidae)
6169090109	AYS	NA (Atylus Swammerdami)
6169120106	APJ	NA (Apherusa Jurinei)
6169150000	COY	NA (Corophiidae)
6169210000	GAY	NA (Gammaridae)
6169220000	HAZ	NA (Haustoriidae)
6169340000	LYX	NA (Lysianassidae)
6169350000	MEZ	NA (Melphidippidae)
6169350101	MPG	NA (Melphidippa Goesi)
6169420000	PAZ	NA (Phoxocephalidae)
6169420104	HPA	NA (Harpina Antennaria)
6169440000	PDX	NA (Podoceridae)
6169440200	LTZ	NA (Laetmatophilus Spp)
6169440201	LEA	NA (Laetmatophilus Armatus)
6169510000	TLZ	NA (Talitridae)
6170010000	HYZ	NA (Hyperiidae)
6170010101	HYG	NA (Hyperia Galba)
6170011000	PTZ	NA (Parathermisto Spp (Thermisto))
6170011005	PAB	NA (Parathermisto(Thermisto)Abyssorum)
6170080000	LYZ	NA (Lycaeidae)
6170150000	SCY	NA (Scinidae)
6171010000	CXZ	NA (Caprellidae)
6171010703	CLL	Ghost Shrimp (Caprella Linearis)
6171010734	CLM	Ghost Shrimp (Caprella Monocera)
6171010735	CPA	Ghost Shrimp (Caprella Punctata)
6171010801	AEL	NA (Aeginina Longicornis)
6171020000	CXX	Parasitic Forms (Cyamidae)
6174020000	KRZ	NA (Euphausiidae)
6174020100	KRX	NA (Euphausia Spp)
6174020200	MPX	NA (Meganectiphanes Spp)
6174020201	MPN	NA (Meganyctiphanes Norvegica)
6174020501	NYC	NA (Nyctiphanes Couchii)
6174020900	THY	NA (Thysanoessa Spp)
6174020902	TYI	NA (Thysanoessa Inermis)
6174020904	TYL	NA (Thysanoessa Longicaudata)
6174020906	TYR	NA (Thysanoessa Raschii)
6175000000	DPD	NA (Decapoda (Order))
6176000000	DCP	(Natantia) (Dendrobranchiata)
6177010000	PEZ	NA (Penaidae)
6177010100	PEN	Penaeid Shrimps (Penaeus Spp)
6177020302	SGA	Penaeid Prawn (Sergestes Arcticus)
6178000000	PLO	NA (Pleocyemata)
6179000000	CAW	NA (Caridea)
6179110000	PAL	Palaemonid Shrimps (Palaemonidae)
6179110200	MCY	NA (Macrobrachium Spp)
6179110802	CPR	Common Prawn (Palaemon Serratus)
6179110804	PSQ	NA (Palaemon Elegans)
6179140701	ASN	NA (Athanas Niteschens)
6179160000	HPX	NA (Hippolytidae)
6179160105	HEV	NA (Hippolyte Varians)
6179160200	SSZ	NA (Spirontocaris Spp)
6179160206	SNG	NA (Spirontocaris (Eualus) Gaimardi)
6179160208	SPL	NA (Spirontocaris Liljeborgii)
6179160216	STN	NA (Spirontocaris Spinus)
6179160217	SCH	NA (Spirontocaris Cranchi)
6179160218	STP	NA (Spirontocaris Pusiola)
6179160400	EUU	NA (Eualus (Genus))
6179160801	CNG	NA (Caridion Gordoni)
6179161300	TOU	NA (Thoralus (Genus))
6179170000	PCY	NA (Processidae)
6179170105	PCC	NA (Processa Canaliculata)
6179180000	PSH	Pandalid Shrimps (Pandalidae (Family))
6179180100	PAN	Pink Shrimps (Nei) (Pandalus Spp)
6179180101	PRA	Northern Pink Shrimp (Pandalus Borealis)
6179180104	PRM	Pink Shrimp (Pandalus Montagui)
6179180110	PDP	NA (Pandalus Propinquus)
6179180200	PSS	NA (P-Pandalopsis (Genus))
6179180302	PDB	NA (Dichelopandalus Bonnieri)
6179180601	PDW	Pink Shrimp (Pandalina  Brevirostris)
6179220000	CRN	Crangonid(Brown)Shrimps (Crangonidae)
6179220100	CSX	NA (Crangon Spp)
6179220118	CSH	Common(Brown)Shrimp (Crangon Crangon)
6179220119	CGA	NA (Crangon Allmanni)
6179220600	PNZ	NA (Pontophilus Spp)
6179220604	PPS	NA (Pontophilus Spinosus)
6179220605	PTF	NA (Pontophilus Fasciatus)
6179220606	PTN	NA (Pontophilus Norvegicus)
6179220701	PPT	NA (Pontophilus(Philocheras)Trispinosus)
6179220702	PPB	NA (Pontophilus(Philocheras)Bispinosus)
6181010000	LOY	Lobsters (Nephropidae)
6181010200	LOX	Lobsters (Nei) (Homarus Spp)
6181010202	LBE	European Lobster (Homarus Gammarus)
6181010301	NEP	Norway Lobster (Nephrops Norvegicus)
6182010000	VLO	Spiny Lobsters (Palinuridae)
6182010100	CRW	Crawfishes (Palinurus Spp)
6182010102	SLS	Crawfish (Palinurus Gilchristi)
6182010301	SLO	Common Spiny Lobster (Palinurus Elephas/Palinurus-Vulgaris)
6182020000	LDS	Slipper Lobsters (Scyllaridae)
6182020204	SLL	Slipper Lobster (Scyllarides Latus)
6183000000	ANZ	NA (Anomura)
6183020000	AXZ	NA (Axiidae)
6183020202	CCM	NA (Calocaris Macandreae)
6183040000	CSZ	NA (Callianassidae)
6183040216	CSB	NA (Callianassa Stebbingi)
6183060000	PAY	Hermit Crabs (Paguridae)
6183060228	PEB	Hermit Crab (Pagurus Bernhardus)
6183060234	PEP	NA (Pagurus Pubescens)
6183061402	APL	NA (Anapagurus Laevis)
6183080000	LTX	NA (Lithodidae)
6183080803	LDM	Stone Crab (Lithodes Maja)
6183100000	LOQ	Squat Lobsters (Galatheidae)
6183100107	MNR	NA (Munida Rugosa)
6183100122	MAS	Squat Lobster (Munida Sarsi)
6183100123	MAT	Squat Lobster (Munida Tenuimana)
6183100300	GLX	Squat Lobsters (Galathea Spp)
6183100302	GLD	NA (Galathea Dispersa)
6183100303	GLS	NA (Galathea Squamifera)
6183100304	GLT	NA (Galathea Strigosa)
6183100305	GLI	NA (Galathea Intermedia)
6183120000	PLY	NA (Porcellanidae)
6183120500	PCL	Porcelain Crab (Porcellana Spp)
6183170103	UPD	NA (Upogebia Deltaura)
6183170105	UPS	NA (Upogebia Stellata)
6184000000	CBX	NA (E-D-Pleocyemata-Brachyura)
6185020202	DRP	Sponge Crab (Dromia Personata)
6185040201	PMC	Box Crab (Paramola Cuvieri)
6186000000	ORX	NA (Oxystomata)
6186030000	LUZ	Nut Crabs (Leucosiidae)
6186030401	EBT	Pennants Nut-Crab (Ebalia Tuberosa)
6186030402	EBM	Bryers Nut-Crab (Ebalia Tumefacta)
6186030407	EBC	Cranchs Nut-Crab (Ebalia Cranchi)
6187010000	MJX	Spider Crabs (Majidae)
6187010202	HYC	Contracted Crab (Hyas Coarctatuss)
6187010203	HYA	Great Spider Crab (Hyas Araneus)
6187011403	RAC	NA (Rochina Carpenteri)
6187012601	EUA	Ruogh Crab (Eurynome Aspera)
6187012602	EUS	NA (Eurynome Spinosa)
6187012701	IND	Scorpion Spider Crab (Inachus Dorsettensis)
6187012702	INP	Leachs Spider Crab (Inachus Phalangium)
6187012703	INL	Slender-Leg Spider Crab (Inachus Leptochirus)
6187013302	MCR	Long-Leg Spider Crab (Macropodia Rostrata)
6187013303	MCT	Slender Spider Crab (Macropodia Tenuirostris)
6187014001	PAA	Gibbs Sea Spider (Pisa Armata)
6187014101	SCR	Spiny Spider Crab (Maia Squinado)
6187014201	DRT	NA (Dorhynchus Thomsoni)
6187014301	ACI	Cranchs Spider Crab (Achaeus Cranchi)
6188010000	CTX	NA (Corystidae)
6188010101	CCV	Masked Crab (Corystes Cassivelaunus)
6188020000	ACX	NA (Atelycyclidae)
6188020301	ALR	Circular Crab (Atelycyclus Rotundatus)
6188030000	CNX	NA (Cancridae)
6188030110	CRE	Edible Crab (Cancer Pagurus)
6188030111	CNB	NA (Cancer Bellanius)
6188040101	PMD	Toothed Pirimela (Pirimela Denticulata)
6189000000	BBZ	NA (Brachyrhyncha)
6189010000	PUZ	NA (Portunidae)
6189010301	CSS	NA (Callinectes Sapidus)
6189010600	CRS	Swimming Crabs (Portunus Spp)
6189010612	PLL	NA (Portumnus Latipes)
6189010701	CRG	Green Shore Crab (Carcinus Maenas)
6189010901	LMD	Swimming Crab (Macropipus (Liocarcinus) Depurator)
6189010902	LMH	Flying Crab (Macropipus (Liocarcinus) Holsatus)
6189010903	LPU	Dwarf-Swimming Crab (Macropipus (Liocarcinus) Pusillus)
6189010904	LMA	Arch-Front Swimming Crab (Macropipus (Liocarcinus) Arcuatus)
6189010905	MPT	NA (Macropipus Tuberculatus)
6189010906	MLP	Velvet Swimming Crab (Macropipus (Liocarcinus) Puber)
6189010907	LMM	Marbled Swimming Crab (Macropipus (Liocarcinus) Marmoreus)
6189021404	PNH	Bristle (Hairy) Crab (Pilumnus Hirtellus)
6189021435	PNS	Hairy Crab (Pilumnus Spinifer)
6189021701	XAP	Rissos Crab (Xantho Pilipes)
6189021702	XAI	Furrowed Crab (Xantho Incisus)
6189040000	GYX	Red Crabs (Geryonidae)
6189040100	GER	Red Crabs (Geryon Spp)
6189040102	GET	Red Crab (Geryon Tridens)
6189040103	GEA	Deep-Water Red Crab (Geryon Affinis)
6189050304	GOR	Angular Crab (Goneplax Rhomboides)
8129030202	AMS	NA (Amphipholis Squamata)
6189060205	PEA	Pea Crab (Pinnotheres Pisum)
6189060207	PTP	Ancient Pea Crab (Pinnotheres Pinnotheres)
6189070501	PTA	Rock Crab (Pachygrapsus Transversa)
6189070701	PNM	Sargassum Crab (Planes Minutus)
6189070801	ERS	Mitten Crab (Eriochir Sinensis)
6189070901	MNC	Mediterranean Crab (Brachynotus Sexdentatus)
6189120101	THS	Polished Crab (Thia Scutellata)
6191000000	STZ	NA (Stomatopoda)
6191010000	SQY	NA (Squillidae)
6191010110	MTS	Mantis Prawn (Squilla Mantis)
7200000000	SIZ	NA (Sipuncula)
7200010000	SIY	NA (Sipunculidae)
7200010101	SIN	NA (Sipunculus Nudus)
7200020000	GFX	NA (Golfingiidae)
7200020102	GFM	NA (Golfingia Margaritacea)
7200020103	GFV	NA (Golfingia Vulgaris)
7200020106	GFN	NA (Golfingia Minuta)
7200020201	OCS	NA (Onchnesoma Squamatum)
7200020401	PCS	NA (Phascolion Strombi)
7200030105	ADM	NA (Aspidosiphon Muelleri)
7300000000	EAZ	NA (Echiura)
7301020000	EAX	NA (Echiuridae)
7301020201	ECE	NA (Echiurus Echiurus)
7400000000	PRZ	NA (Priaptulida)
7400010000	PRY	NA (Priaptulidae)
7400010101	PPC	NA (Priaptulus Caudatus)
7800000000	EPZ	Sea Mats (Ectoprocta/ Bryozoa)
7803010000	ALX	NA (Alcyonidiidae)
7803010110	ALG	Curly Weed (Alcyonidium Gelatinosum)
7803010114	ALQ	NA (Alcyonidium Proboscideum)
7813010401	MTB	NA (Myriopora (Coronopora) Truncata)
7815060000	FAX	NA (Flustridae)
7815060103	FAF	Hornwrack (Flustra Foliacea)
7815060301	FAS	Hornwrack (Securiflustra Securifrons)
7816150000	RXX	NA (Reteporidae(Sertellidae))
7816150501	SBN	NA (Sertella (Retepora) Beaniana)
8000000000	BCZ	NA (Brachiopoda)
8010000000	CHZ	Green Seaweeds (Chlorophyceae)
8012020101	PDA	NA (Pelagodiscus Atlanticus)
8013020101	CNN	NA (Crania Anomala)
8014000000	ATY	NA (Articulata)
8015040101	HTP	NA (Hemithiris Psittacea)
8017020101	GYV	NA (Gryphus Vitreus)
8017050000	CCY	NA (Cancellothyrididae)
8017050105	TTR	NA (Terebratulina Retusa)
8018020101	AGC	NA (Argyrotheca Cistellula)
8018030101	PAO	NA (Platidia Anomoides)
8018050101	DLS	NA (Dallina Septigera)
8050305030	ULL	Sea Lettuce (Ulva Lactuca)
8100000000	ECH	Starfish/Sea Urchns (Echinodermata)
8104000000	STF	Starfishes (Asteroidea)
8105000000	APZ	NA (Platyasterida)
8105010000	LAX	NA (Luidiidae)
8105010105	LUS	NA (Luidia Sarsi)
8105010106	LDC	NA (Luidia Ciliaris)
8106000000	APD	NA (Paxillosida-Diplozonina)
8106010000	APX	NA (Asteropectinidae)
8106010304	PSA	NA (Psilaster Andromida)
8106010401	BBV	NA (Bathybiaster Vexillifer)
8106010508	API	NA (Astropecten Irregularis)
8106010509	APA	NA (Astropecten Arantciacus)
8106010701	DYG	NA (Dytaster Grandis)
8108000000	APN	NA (Notomyotina)
8108010000	BPX	NA (Benthopectinidae)
8108010104	BPT	NA (Benthopecten Armatus)
8108010501	PAT	NA (Pontaster Tenuispinas)
8111000000	AVG	NA (A-Valvatida-Granulosina)
8111040000	GAX	NA (Goniasteridae)
8111040107	CMG	NA (Ceramaster Granulosus)
8111040109	CMP	NA (Ceramaster Placenta)
8111040404	HPP	NA (Hippasteria Phrygiana)
8111040602	PSP	NA (Pseudarchaster Parelii)
8111060000	OPY	NA (Ophidiasteridae)
8111060402	OPO	NA (Ophidiaster Ophidianus)
8112000000	SPI	NA (Spinulosida (Order))
8113000000	ASF	NA (Spinulosida-Eugnathina)
8113010000	SAZ	NA (Solasteridae)
8113010103	CTP	NA (Crossaster Papposus)
8113010302	SLE	NA (Solaster Endeca)
8113040000	PTX	NA (Pterasteridae)
8113040204	HYP	NA (Hymenaster Pellucidus)
8113040205	HYM	NA (Hymenaster Membranaceus)
8113040302	PTM	NA (Pteraster Militaris)
8114000000	ASZ	NA (Spinulosida-Leptognatha)
8114010000	ASX	NA (Asterinidae)
8114010102	ATG	Cushion Star (Asterina Gibbosa)
8114010301	PLM	Goose-Foot Star (Anseropoda Placenta)
8114030000	PPY	NA (Poraniidae)
8114030202	PMH	NA (Poraniomorpha Hispida)
8114030302	PPV	NA (Porania Pulvillus)
8114040000	ECX	NA (Echinasteridae)
8114040100	HEX	NA (Henricia Spp)
8114040111	HNS	NA (Henricia Sanguinolenta)
8114040116	HEO	NA (Henricia Oculata)
8114040303	ECS	NA (Echinaster Sepositus)
8115000000	FOA	NA (Forcipulatida)
8116000000	AFA	NA (Asteriadina)
8117020000	ZAX	NA (Zoasteriidae)
8117030000	ATX	NA (Asteriidae)
8117030205	STH	Common Starfish (Asterias Rubens)
8117030416	LSM	NA (Leptasterias Mulleri)
8117031701	MAG	Spiny Starfish (Marthasterias Glacialis)
8117032001	SLR	NA (Stichastrella Rosea)
8120000000	BSY	Brittle-Stars (Ophiuroidea)
8126000000	OPH	NA (Ophiurida (Order))
8127000000	BOC	NA (Chilophiurina)
8127010000	BTZ	Brittle-Stars (Ophiuridae)
8127010610	OPS	NA (Ophiura Sarsi)
8127010612	OHA	NA (Ophiura Albida)
8127010614	OHT	NA (Ophiura Texturata)
8127010701	OPL	NA (Ophiomusium Lymani)
8127030000	OCX	NA (Ophiocomidae)
8127030201	OPN	NA (Ophiocomina Nigra)
8127050000	ODX	NA (Ophiodermatidae)
8127050107	ODL	NA (Ophioderma Longicauda)
8129000000	OGX	NA (O-Gnathophiurina)
8129020000	OPX	NA (Ophiactidae)
8129020101	OAC	NA (Ophiopholis Aculeata)
8129030000	AMY	NA (Amphiuridae)
8129031005	AMC	NA (Amphiura Chiajei)
8129031006	AMF	NA (Amphiura Filiformis)
8129031024	AAB	NA (Amphiura Brachiata)
8129040000	OPZ	NA (Ophiothricidae)
8129040103	OPF	Common Brittle Star (Ophiothrix Fragilis)
8135000000	ECZ	NA (Echinozoa)
8136000000	URX	Sea Urchins (Nei) (Echinoidea)
8138000000	ECY	NA (E-Cidaroida)
8138010000	CIY	NA (Cidaridae)
8138010301	STA	NA (Stylocidaris Affinis)
8138010402	CDC	NA (Cidaris Cidaris)
8147000000	EAY	NA (E-Arbacioida)
8147010000	ABY	NA (Arbaciidae)
8147010102	ABL	NA (Arbacia Lixula)
8149000000	EEZ	Sea Urchins (E-Echinoida)
8149010000	EEX	NA (Echinidae)
8149010101	URP	NA (Strongylocentrotus Lividus)
8149010201	URA	NA (Echinus Acutus)
8149010202	URS	Edible Sea Urchin (Echinus Esculentus)
8149010203	URM	NA (Echinus Melo)
8149010204	URD	Deep-Sea Urchin (Echinus Affinis)
8149010301	PMM	Green Sea Urchin (Psamechinus Miliaris)
8149030000	USX	NA (Strongylocentrotidae)
8149030201	USD	NA (Strongylocentrotus Drobachiensis)
8154000000	ECL	NA (E-Laganina)
8154010000	FIX	NA (Fibulariidae)
8154010101	ECP	NA (Echinocyamus Pusillus)
8160000000	ESZ	NA (E-Spatangoida)
8160101000	BRY	NA (Bryopsis Spp)
8163000000	ESM	NA (E-Micrasterina)
8163010000	BRX	NA (Brissidae)
8163010303	BRL	Lyre-Urchin (Brissopsis Lyrifera)
8163010401	BRU	NA (Brissus Unicolor)
8163020000	STY	NA (Spatangidae)
8163020101	SPG	Purple Heart Urchin (Spatangus Purpureus)
8163020102	SRI	Heart Urchin (Spatangus Raschii)
8163030000	LOV	NA (Loveniidae)
8163030101	ECC	Sea Potato (Echinocardium Cordatum)
8163030102	ECF	NA (Echinocardium Flavescens)
8163030103	ECN	NA (Echinocardium Pennatifidum)
8170000000	HTZ	Sea Cucumbers (Holothuroidea)
8171000000	HDZ	NA (H-Dendrochirotacea)
8172000000	HDD	NA (H-D-Dendrochirotida)
8172030000	PSX	NA (Psolidae)
8172030205	POP	NA (Psolus Phantapus)
8172060000	CMX	NA (Cucumariidae)
8172060104	CUF	NA (Cucumaria Frondosa)
8172060118	CUE	NA (Cucumaria Elongata)
8172060120	CUM	NA (Cucumaria Normani)
8172060504	THI	NA (Thyone Inermis)
8172060505	THH	NA (Thyone Fusus)
8174000000	HAY	NA (H-Aspidochirotacea)
8175000000	HAA	NA (H-A-Aspidochirotida)
8175020000	SDZ	NA (Stichopodidae)
8175020201	STT	NA (Stichopus Tremulus)
8175020202	STQ	NA (Stichopus Fusus)
8175020203	SSR	NA (Stichopus Regalis)
8177000000	HPZ	NA (H-Apodacea)
8178000000	AAY	NA (H-A-Apodida)
8178010102	LPD	NA (Labidoplax Digitata)
8178010201	LEI	NA (Leptosynapta Inhaerans)
8185000000	CZZ	NA (Crinozoa)
8186000000	CNZ	NA (Crinoidea)
8189000000	COM	NA (Comatulida)
8191000000	CMZ	NA (C-Macrophreata)
8191010000	ADX	NA (Antedonidae)
8191010401	ADB	Feather Star (Antedon Bifida)
8191010701	LMC	NA (Leptometra Celtica)
8200000000	HCZ	NA (Hemichordata)
8201000000	ETZ	NA (Enteropneusta)
8201020000	PTY	NA (Ptychoderidae)
8201020102	BAC	Acorn Worm (Balanoglossus Clavigerus)
8300000000	CTZ	Arrow Worms (Chaetognatha)
8306030101	EUH	Arrow Worm (Eukrohnia Hamata)
8308010701	SGE	Arrow Worm (Sagitta (Parasagitta) Elegans)
8308010707	SGT	Arrow Worm (Sagitta (Parasagitta) Setosa)
8388000000	CCX	NA (Chordata)
8400000000	URZ	NA (Urochordata (Tunicates))
8401000000	SSX	Sea Squirts (Ascidiacea)
8402000000	AGZ	NA (A-Enterogona)
8403000000	EGZ	NA (A-E-Aplousobranchia)
8403010000	CVX	NA (Clavelinidae)
8403010203	CVL	NA (Clavelina Lepadiformis)
8403010405	ARA	NA (Archidistoma Aggregatum)
8403020000	PCZ	NA (Polyclinidae)
8403020103	AAG	NA (Amaroucium Glabrum)
8403020114	AAP	NA (Amaroucium Proliferum)
8403020201	SYQ	NA (Synoicum Pulmonaria)
8403020403	PCA	NA (Polyclinum Aurantium)
8403020802	SIT	NA (Sidnyum Turbinatum)
8403030000	DIX	NA (Didemnidae)
8403030104	DIH	NA (Didemnum Helgolandicum)
8403030402	DPL	NA (Diplosoma Listerianum)
8404000000	APY	NA (Ascidiacea-Phlebobranchiata)
8404010000	CNY	NA (Cionidae)
8404010101	CNI	NA (Ciona Intestinalis)
8404020000	DIZ	NA (Diazonidae)
8404020201	DIV	NA (Diazona Violacea)
8404030000	PHX	NA (Perophoridae)
8404030103	PPL	NA (Perophora Listeri)
8404040000	CLY	NA (Corellidae)
8404040203	CLP	NA (Corella Parallelogramma)
8404050000	ASY	NA (Ascidiidae)
8404050101	ASU	NA (Ascidia Prunum)
8404050103	ASO	NA (Ascidia Obliqua)
8404050108	ASM	NA (Ascidia Mentula)
8404050109	ASD	NA (Ascidia Conchilega)
8404050110	ASV	NA (Ascidia Virginea)
8404050201	ASB	NA (Ascidiella Aspersa)
8404050202	ASS	NA (Ascidiella Scabra)
8405000000	APW	NA (Ascidiacea-Pleurogona)
8406000000	PSY	NA (A-Pleurogona-Stolidobranchiata)
8406010000	SYX	NA (Styelidae)
8406010303	CNM	NA (Cnemidocarpa Mollis)
8406010404	DDG	NA (Dendroda Grossularia)
8406010502	SYC	NA (Styela Coriacea)
8406010509	SEP	NA (Styela Partita)
8406010601	PCG	NA (Pelonaia Corrugata)
8406010801	PCF	NA (Polycarpa Fibrosa)
8406010803	PCP	NA (Polycarpa Pomaria)
8406010900	POZ	NA (Polyzoa (Genus))
8406020000	PZX	NA (Pyruidae)
8406020105	PYT	NA (Pyrua Tessellata)
8406020202	BOE	NA (Boltenia Echinata)
8406020701	MIC	NA (Microcosmus Claudicans)
8406030000	MGX	NA (Molgulidae)
8406030108	MGM	NA (Molgula Manhattensis)
8406030111	MGC	NA (Molgula Citrina)
8406030112	MGO	NA (Molgula Oculata)
8406030114	MGL	NA (Molgula Complanata)
8406030402	EYA	NA (Eugyra Arenosa)
8500000000	CCA	NA (Cephalocordata)
8500010000	BNS	NA (Branchiostomidae)
8500010301	LCT	Lancet (Amphioxus Lanceolatus)
8600000000	AGN	Fish Without Jaws (Agnatha)
8603000000	PMF	Hagfish And Lampreys (Petromyzoniformes)
8603010000	LAS	Lampreys (Petromyzonidae)
8603010217	LAR	River Lamprey (Lampetra Fluviatilis)
8603010218	LAB	Brook Lamprey (Lampetra Planeri)
8603010300	LAM	Lampreys-Marine (Petromyzon Spp)
8603010301	SLY	Sea Lamprey (Petromyzon Marinus)
8606010201	HGF	Hagfish (Myxine Glutinosa)
8703000000	SKX	Sharks-Skates-Rays Etc (Selachiomorpha)
8705010101	FDS	Frilled Shark (Chlamydoselachus Anguineus)
8705020101	SGS	Six-Gilled Shark (Hexanchus Griseus)
8707040302	POR	Porbeagle Shark (Lamna Nasus)
8707040401	ATH	Thresher Shark (Alopias Vulpinus)
8707040501	SMA	Mako Shark (Isurus Oxyrinchus)
8707120100	BSC	NA (Cetorhinidae)
8707120101	BSK	Basking Shark (Cetorhinus Maximus)
8708000000	DGY	NA (Lamniformes)
8708010000	DGH	Dogfishes (Scyliorhinidae)
8708010103	DAL	NA (Apristurus Laurussoni)
8708010203	DBM	Blackmouthed Dogfish (Galeus Melastomus)
8708010204	DGM	NA (Galeus Murinus)
8708010300	SHT	Dogfishes (Scyliorhinus Spp)
8708010306	LSD	Lesser Spotted Dogfish (Scyliorhinus Canicula)
8708010307	DGN	Nurse Hound (Scyliorhinus Stellaris)
8708020102	GAG	Tope Shark (Galeorhinus Galeus)
8708020201	TIG	Tiger Shark (Galeocerdo Cuvier)
8708020408	SDS	Starry Smooth Hound (Mustelus Asterias)
8708020409	SMH	Smooth Hound (Mustelus Mustelus)
8708020410	SMP	NA (Mustelus Punctulatus)
8708020601	BSH	Blue Shark (Prionace Glauca)
8708030102	HSH	Common Hammerhead Shark (Sphyrna Zygaena)
8708030103	HHS	Hammerhead Shark (Sphyrna Lewini)
8708030105	GHS	Great Hammerhead Shark (Sphyrna Tudes)
8708070100	FCS	NA (Pseudotriakis)
8708070101	FCK	False Catshark (Pseudotriakis Microdon)
8709000000	SHX	NA (Squaliformes)
8710000000	SHQ	NA (Squaloidei)
8710010000	DGZ	Squalid Sharks (Squalidae)
8710010102	GSK	Greenland Shark (Somniosus Microcephalus)
8710010201	DGS	Spurdog (Squalus Acanthias)
8710010202	DGB	NA (Squalus Blainvillei)
8710010301	CGS	NA (Centrophorus Granulosus)
8710010302	CSQ	NA (Centrophorus Squamosus)
8710010303	CUY	NA (Centrophorus Uyato)
8710010401	DCH	Darkie Charlie (Dalatias (Scymnorhinus) Licha)
8710010503	ESP	NA (Etmopterus Princeps)
8710010510	VBY	Velvet Belly (Etmopterus Spinax)
8710010702	HTN	Humantin (Oxynotus Centrina)
8710010703	SSK	Sharpback Shark (Oxynotus Paradoxus)
8710010901	CSF	NA (Centroscyllium Fabricii)
8710011201	PUS	Portuguese Shark (Centroscymnus Coelolepis)
8710011202	CMS	NA (Centroscymnus Crepidater)
8710011401	DAC	NA (Daenia Calceus)
8710011601	SNR	NA (Scymnodon Ringens)
8710011602	SNO	NA (Scymnodon Obscurus)
8710020204	DGF	NA (Squalus Fernandinus)
8710030000	SYE	NA (Echinorhinidae)
8710030100	SYH	NA (Echinorhinus Spp)
8710030101	SYS	Spiny Shark (Echinorhinus Brucus)
8711010000	ASK	Angel Sharks (Squatinidae)
8711010103	ALS	Angelshark (Monkfish) (Squatina Squatina)
8712000000	SRB	NA (Batoidimorpha)
8713000000	SRX	NA (Rajiformes)
8713030102	ECR	Common Electric Ray (Torpedo Nobiliana)
8713030104	ELR	Electric Ray (Torpedo Torpedo)
8713030105	MER	Marbled Electric Ray (Torpedo Marmorata)
8713040000	SKA	Skates And Rays (Rajidae)
8713040100	SRY	Skates And Rays (Raja Spp)
8713040134	SYR	Starry Ray (Raja Radiata)
8713040138	BLR	Blonde Ray (Raja Brachyura)
8713040140	PTR	Painted Ray (Raja Microocellata)
8713040141	SDR	Spotted Ray (Raja Montagui)
8713040142	ACS	Arctic Skate (Raja Hyperborea)
8713040143	SKT	Common Skate (Raja Batis)
8713040144	RNS	NA (Raja Nidarosiensis)
8713040145	LNS	Long-Nose Skate (Raja Oxyrinchus)
8713040146	SHR	Shagreen Ray (Raja Fullonica)
8713040147	SAR	Sandy Ray (Raja Circularis)
8713040148	CUR	Cuckoo Ray (Raja Naevus)
8713040150	RDS	Round Skate (Raja Fyllae)
8713040151	WSK	White Skate (Raja Alba)
8713040153	RLS	NA (Raja Lintea)
8713040158	UNR	Undulate Ray (Raja Undulata)
8713040159	THR	Thornback Ray (Roker) (Raja Clavata)
8713040801	BRP	NA (Bathyraja (Breviraja) Pallida)
8713040803	BAS	Arctic Ray (Bathyraja Spinicauda)
8713050111	SGR	Sting Ray (Dasyatis Pastinacus)
8713070204	EGR	Eagle Ray (Mylobatis Aquila)
8713080205	DVR	Devil Ray (Mobula Mobular)
8716020000	RRX	Rabbit Fishes/Ratfishes (Chimaeridae)
8716020103	RTF	Ratfish (Hydrolagus Mirabilis)
8716020202	RBF	Rabbit Fish(Rat-Tail) (Chimaera Monstrosa)
8716030201	RHA	NA (Rhinochimaera Atlantica)
8717000000	FIS	Boney Fish (Osteichthys)
8729010000	STX	NA (Acipenseridae)
8729010107	STU	(European)Sturgeon (Acipenser Sturio)
8741010000	EEL	Eels (Anguillidae)
8741010100	ELX	NA (Anguilla Spp)
8741010102	ELE	European Eel (Anguilla Anguilla)
8741050000	MRX	Moray Eels (Muraenidae)
8741050505	MRY	Moray Eel (Muraena Helena)
8741120000	COX	Conger Eels (Congridae)
8741120111	COE	European Conger Eel (Conger Conger)
8741150104	SBK	NA (Synaphobranchus Kaupi)
8741200102	SVB	NA (Serrivomer Beani)
8741200104	SVP	NA (Serrivomer Parabeani)
8741210000	SPX	Snipe Eels (Nemichthyidae)
8741210102	ATI	NA (Avocettina Infans)
8741210202	SPE	Snipe Eel (Nemichthys Scolopaceus)
8743030000	SEX	Spiny Eels (Notacanthidae)
8743030204	RSE	Risso'S Spiny Eel (Polyacanthonotus Rissoanus)
8743030301	CPS	Chemnitz'S Spiny-Eel (Notacanthus Chemnitzii)
8743030302	BPS	Bonapart'S Spiny-Eel (Notacanthus Bonaparti)
8747000000	DCX	NA (Clupeiformes-Clupeoidei)
8747010000	CLU	Herrings (Clupeidae)
8747010100	SHD	Shads (Alosa Spp)
8747010107	AAS	Allis Shad (Alosa Alosa)
8747010109	TAS	Twaite Shad (Alosa Fallax)
8747010201	HER	Herring (Clupea Harengus)
8747011701	SPR	Sprat (Sprattus (Clupea) Sprattus)
8747012201	PIL	Pilchard (Sardina (Clupea) Pilchardus)
8747020104	ANE	European Anchovy (Engraulis Encrasicolus)
8755000000	SLX	NA (Salmoniformes-Salmonoidei)
8755010000	SLZ	Salmonids (Salmonidae)
8755010100	WHF	Whitefish (Coregonus Spp)
8755010115	PLN	Pollan(Fw Houting) (Coregonus Lavaretus)
8755010116	FVE	Powan(Vendace) (Coregonus Albula)
8755010200	PNK	Pink Salmon (Oncorhynchus Spp)
8755010201	PIN	Pink Salmon (Oncorhynchus Gorbuscha)
8755010202	CHM	Chum Salmon (Oncorhynchus Keta)
8755010211	TRB	Rainbow Trout (Oncorhynchus Mykiss)
8755010300	TRO	Trouts (Salmo Spp)
8755010305	SAL	N.Atlantic Salmon (Salmo Salar)
8755010306	TRS	Sea Trout (Brown Trout) (Salmo Trutta)
8755010400	CHR	Chars (Salvelinus Spp)
8755010402	ACH	Char (Salvelinus Alpinus)
8755010404	BKT	Brook Char (Salvelinus Fontinalis)
8755010704	FGR	Grayling (Thymallus Thymallus)
8755010801	HUC	Huchen (Hucho Hucho)
8755030100	SMX	NA (Hypomesus Spp)
8755030201	CAP	Capelin (Mallotus Villosus)
8755030300	SMO	NA (Osmerus Spp)
8755030301	SME	Smelt(Sparling) (Osmerus Eperlanus)
8756010000	ARG	Argentines (Argentinidae)
8756010203	GSS	Gt Silver Smelt (Argentina Silus)
8756010209	LSS	Lsr Silver Smelt (Argentina Sphyraena)
8758010101	FPI	European Pike (Esox Lucius)
8758020101	AMM	Eastern Mud Minnow (Umbra Pygmaea)
8758020103	EMM	European Mud Minnow (Umbra Krameri)
8759010501	PLS	Pearlside (Maurolicus Muelleri)
8759020107	HTF	Hatchet Fish (Argyropelecus Olfersi)
8760010000	SMY	Smooth Heads (Alepocephalidae)
8760010302	ROH	Risso'S Smooth Head (Alepocephalus Rostratus)
8760010305	BSD	Baird'S Smooth Head (Alepocephalus Bairdii)
8760010704	CNS	NA (Conocara Salmonea)
8762070201	PLR	NA (Paralepis (Notolepis) Rissoi)
8762070402	PLC	NA (Paralepis Coregonoides)
8762140000	MYX	Lantern Fishes (Myctophidae)
8762140300	LNX	Lantern Fishes (Lampanyctus Spp)
8762140317	LAC	Lantern Fish (Lampanyctus Crocodilus)
8776010000	FCY	Cyprinids (Cyprinidae)
8776010101	FCP	Common Carp (Cyprinus Carpio)
8776010201	FTE	Tench (Tinca Tinca)
8776010301	FGF	Gold Fish (Carassius Auratus)
8776010302	FCC	Crucian Carp (Carassius Carassius)
8776013201	FID	Orfe (Ide) (Leuciscus Idus)
8776013202	FDC	Dace (Leuciscus Leuciscus)
8776013203	CUB	Chub (Leuciscus Cephalus)
8776013706	FMW	Minnow (Phoxinus Phoxinus)
8776014101	FBT	Bitterling (Rhodeus (Sericeus) Amarus)
8776014201	FRD	Rudd (Scardinius Erythrophthalmus)
8776014401	FRO	Roach (Rutilus Rutilus)
8776014501	FBL	Barbel (Barbus Barbus)
8776014601	FGN	Gudgeon (Gobio Gobio)
8776014701	FSB	Silver Bream (Blicca (Alburnus) Bjoerkna)
8776014801	FBK	Bleak (Alburnus Alburnus)
8776014900	FBR	Breams (Abramis Spp)
8776014901	FBM	Bream (Abramis Brama)
8776060000	FLZ	Loaches (Cobitidae)
8776060201	FSL	Spined Loach (Cobitis Taenia)
8776060301	FTL	Stone Loach (Noemacheilus Barbatulus)
8777020605	FHP	Horned Pout (Ictalurus Nebulosus)
8777050000	FSI	Fw Catfishes (Siluridae)
8777050101	SOM	Fw Catfish (Wels) (Silurus Glanis)
8784010000	CFX	Clingfishes (Gobiesocidae)
8784010601	CAC	Connemarra Clingfish (Lepadogaster Candollei)
8784010603	SCF	Shore Clingfish (Lepadogaster Lepadogaster)
8784010701	TSC	Twp Spotted Clingfish (Diplecogaster Bimaculata)
8784010801	SCL	Small-Headed Clingfish (Apletodon Microcephalus)
8786010000	ANX	Angler Fishes (Lophiidae)
8786010100	ANF	Anglers (=Monk) (Lophius Spp)
8786010103	MON	Anglerfish (Monk) (Lophius Piscatorius)
8786010104	WAF	Black-Bellied Anglerfish (Lophius Budegassa)
8787020101	HOH	NA (Histrio Histrio)
8787020203	ASR	NA (Antennarius Radiosus)
8788030201	DAF	Atlantic Football-Fish (Himantolophus Groenlandicus)
8788080101	DAH	Deep-Sea Anglerfish (Ceratias Holboelli)
8788100102	LEL	NA (Linophryne Lucifer)
8791010000	MOR	Morid Cods (Moridae)
8791010101	BAN	Blue Antimora (Antimora Rostrata)
8791010203	LML	NA (Laemonema Latifrons)
8791010401	MOM	NA (Mora Moro)
8791010501	LPE	NA (Lepidion Eques)
8791010601	HGA	NA (Halargyreus Affinis (H.Johnsonii))
8791030000	GAD	Codlike Fishes (Gadidae)
8791030201	POC	Arctic Cod (Boreogadus Saida)
8791030402	COD	Cod (Gadus Morhua)
8791030801	FBU	Burbot (Lota Lota)
8791030901	POK	Saithe (Pollachius Virens)
8791030902	POL	Pollack (Pollachius Pollachius)
8791031101	USK	Tusk (Brosme Brosme)
8791031301	HAD	Haddock (Melanogrammus Aeglefinus)
8791031500	FRG	NA (Enchelyopus)
8791031501	FRR	Four-Bearded Rockling (Enchelyopus Cimbrius)
8791031602	GFB	Greater Forkbeard (Phycis Blennoides)
8791031701	POD	Poor Cod (Trisopterus Minutus)
8791031702	BIB	Whiting-Pout (Bib) (Trisopterus Luscus)
8791031703	NOP	Norway Pout (Trisopterus Esmarki)
8791031801	WHG	Whiting (Merlangius Merlangus)
8791031901	LIN	Common Ling (Molva Molva)
8791031902	BLI	Blue Ling (Molva Dypterygia)
8791031904	SLI	Spanish Ling (Molva Macrophthlma)
8791032000	ROL	Rocklings (Gaidropsarus Spp)
8791032001	TBR	Three-Bearded Rockling (Gaidropsarus Vulgaris)
8791032002	SRR	Shore Rockling (Gaidropsarus Mediterraneus)
8791032101	SYP	Silvery Pout (Gadiculus Argenteus)
8791032201	WHB	Blue Whiting (Micromesistius Poutassou)
8791032301	LFB	Lesser Forkbeard (Raniceps Raninus)
8791032401	FVR	Five-Bearded Rockling (Ciliata Mustela)
8791032402	NNR	Northern Rockling (Ciliata Septentrionalis)
8791032501	OGA	NA (Onogadus Argenteus)
8791032601	AGM	NA (Antonogadus Macropthalmus)
8791040000	HKY	Hakes (Merlucciidae)
8791040105	HKE	European Hake (Merluccius Merluccius)
8792010607	OPB	NA (Ophidion Barbatum)
8792020000	PRX	Pearlfishes (Carapidae)
8792020202	PRL	Pearlfish (Echiodon Drummondi)
8793010000	EPX	Eel-Pouts (Zoarcidae)
8793010513	LCS	NA (Lycenchelys Sarsi)
8793010700	POU	Eelpouts (Lycodes spp.)
8793010724	VLP	Vahl'S Eelpout (Lycodes Vahlii)
8793010725	EEP	Esmark'S Eelpout (Lycodes Esmarkii)
8793012001	ELP	Eelpout/Viviparus Blenny (Zoarces Viviparus)
8794010000	RTX	Rattails (Macrouridae)
8794010100	RNX	NA (Coryphaenoides Spp)
8794010117	RNG	Roundnose Grenadier (Coryphaenoides Rupestris)
8794010405	HRT	Hollow Nosed Rattail (Coelorinchus Coelorhinchus)
8794010601	SRT	Soft Headed Rattail (Malacocephalus Laevis)
8794010801	SRL	Smooth Rattail (Nezumia Aequalis)
8794011501	RNR	Roughnosed Rattail (Trachyrhynchus Trachyrhynchus)
8794011502	MYR	Murray'S Rattail (Trachyrhynchus Murrayi)
8794011601	RHG	Rough Rattail (Macrourus Berglax)
8803010000	FLY	Flying Fishes (Exocoetidae)
8803010101	AFF	Atlantic Flying Fish (Cypselurus Heterurus)
8803010106	CPB	NA (Cypselurus Pinnatibarbatus)
8803010501	DSR	NA (Danichthys Rondeletii)
8803010701	EXO	NA (Exocoetus Obtusirostris)
8803020000	BEX	NA (Belonidae)
8803020502	GAR	Garfish (Belone Belone)
8803030201	SAU	Saurey Pike (Scomberesox Saurus)
8805020000	SIL	Sand-Smelts (Atherinidae)
8805021002	SMI	NA (Atherina Boyeri)
8805021003	SMT	Sand Smelt (Atherina Presbyter)
8806000000	DPX	NA (Acanthopterygii)
8810010101	DSA	NA (Diretmus Argenteus)
8810020101	DIR	NA (Gephyroberyx Darwini)
8810020201	RHF	Orange Rough-Fish (Hoplostethus Atlanticus)
8810020202	PIK	Pink Rough-Fish (Hoplostethus Mediterraneus)
8810050000	BYX	NA (Berycidae)
8810050101	BER	Beryx (Beryx Decadactylus)
8810050102	LWB	Lowes Beryx (Beryx Splendens)
8811030301	JOD	John Dory (Zeus Faber)
8811060301	BOF	Boar Fish (Capros Aper)
8813010102	OPA	Opah (Moon-Fish) (Lampris Guttatus)
8815020102	DLF	Deal Fish (Trachipterus Arcticus)
8815030101	RNF	Ribbon Fish (Regalecus Glesne)
8818010000	SKB	Sticklebacks (Gasterosteidae)
8818010101	TSS	Three-Spined Stickleback (Gasterosteus Aculeatus)
8818010201	TNS	Ten-Spined Stickleback (Pungitius Pungitius)
8818010501	SSS	Sea Stickleback (Spinachia Spinachia)
8819030101	SNI	Snipe-Fish (Macrorhamphosus Scolopax)
8820020000	PFX	Pipe-Fishes/Seahorses (Syngnthidae)
8820020119	NPF	Nilsson'S Pipefish (Syngnathus Rostellatus)
8820020120	GPF	Great Pipefish (Syngnathus Acus)
8820020123	DPF	Deep-Snouted Pipefish (Syngnathus Typhle)
8820020209	SNH	Sea Horse (Short Snouted) (Hippocampus Hippocampus)
8820020210	SHE	Sea Horse (Hippocampus Ramulosus)
8820022101	SKP	Snake Pipefish (Entelurus Aequoreus)
8820022201	WPF	Worm Pipefish (Nerophis Lumbriciformis)
8820022202	SNP	Straight-Nosed Pipefish (Nerophis Ophidion)
8825000000	SFZ	NA (Scorpaeniformes)
8826000000	SFY	NA (Scorpaenoidei)
8826010000	SCO	Scorpion Fishes (Scorpaenidae)
8826010100	REG	Redfishes (Sebastes Spp)
8826010139	RED	Redfish (Sebastes Marinus)
8826010151	REB	Rose Fish (Sebastes Mentella)
8826010175	REV	Redfish (Sebastes Viviparus)
8826010301	RBM	Blue-Mouth Redfish (Helicolenus Dactylopterus)
8826010628	SCS	NA (Scorpaena Scrofa)
8826010629	SPP	NA (Scorpaena Porcus)
8826011101	SNF	Scorpion Fish (Trachyscorpia Cristulata)
8826020000	ROX	E.Atlantic Gurnards (Triglidae)
8826020316	AGD	Armed Gurnard (Peristedion Cataphractum)
8826020500	GUX	Gurnards (Trigla Spp)
8826020501	TUB	Tub Gurnard (Trigla Lucerna)
8826020503	PIP	Piper (Trigla Lyra)
8826020601	GUG	Grey Gurnard (Eutrigla Gurnardus)
8826020701	GUS	Streaked Gurnard (Trigloporus Lastoviza)
8826020801	GUR	Red Gurnard (Aspitrigla Cuculus)
8826020802	GUL	Long-Finned Gurnard (Aspitrigla Obscura)
8831000000	SFX	NA (Cottoidei)
8831020000	CDY	Bullheads And Sculpins (Cottidae)
8831020308	ARE	NA (Artediellus Europaeus)
8831020825	BUL	Bullhead (Cottus Gobio)
8831022200	SCU	Sculpin (Myoxocephalus Spp)
8831022205	MQS	Four Spined Sculpin (Myoxocephalus Quadricornis)
8831022207	BRT	Bullrout (Myoxocephalus Scorpius)
8831023807	SPN	Sculpin (Triglops Murrayi)
8831024601	SSN	Sea Scorpion (Taurulus Bubalis)
8831024602	NVB	Norway Bullhead (Taurulus Lilljeborgi)
8831060101	CTM	NA (Cottunculus Microps)
8831080000	AGY	NA (Agonidae)
8831080803	POG	Pogge (Armed Bullhead) (Agonus Cataphractus)
8831081801	ATP	Atlantic Poacher (Leptagonus Decagonus)
8831090000	CYW	NA (Cyclopteridae)
8831090232	CPL	NA (Careproctus Longipinnis)
8831090234	CSR	NA (Careproctus Reinhardi)
8831090800	LPS	Sea Snails (Liparis Spp)
8831090828	SSL	Sea Snail (Liparis Liparis)
8831090831	MSS	Montague'S Seasnail (Liparis Montagui)
8831091501	LUM	Lumpsucker (Cyclopterus Lumpus)
8834000000	PPX	NA (Perciformes)
8835000000	BSZ	NA (Percoidei)
8835020000	BSX	Basses And Sea-Perches (Serranidae)
8835020435	GPD	Dusky Perch (Grouper) (Epinephelus Guaza)
8835022316	CMR	Comber (Serranus Cabrilla)
8835022801	WKF	Wreck-Fish (Polyprion Americanus)
8835160000	CTY	NA (Centrarchidae)
8835160201	RBS	Rock Bass (Ambloplites Rupestris)
8835160505	LMG	NA (Lepomis Gibbosus)
8835160601	SMB	Small-Mouthed Bass (Micropterus Dolomieui)
8835160602	LMB	Large-Mouthed Bass (Micropterus Salmoides)
8835180000	AGW	NA (Apogonidae)
8835180403	EGT	NA (Epigonus Telescopus)
8835181201	RHS	NA (Rhectogramma Sherborni)
8835200000	PCW	NA (Percidae)
8835200202	FPE	European Perch (Perca Fluviatilis)
8835200403	FPP	Zander (Pike Perch) (Stizostedion Lucioperca)
8835200601	FRU	Ruff (Pope) (Gymnocephalus Cernuus)
8835270000	ECW	NA (Echeneidae)
8835270103	SKS	Shark Sucker (Remora Remora)
8835280000	CGX	NA (Carangidae)
8835280100	JAX	Jack-Mackerels (Trachurus Spp)
8835280103	HOM	Horse-Mackerel (Scad) (Trachurus Trachurus)
8835280105	HMM	Mediterranean Scad (Trachurus Mediterraneus)
8835280106	JAA	Blue Scad (Trachurus Picturatus)
8835280801	YWT	Yellow Tail (Seriola Dumerili)
8835280900	POX	NA (Trachinotus Spp)
8835280911	DIO	Derbio (Trachinotus Ovatus)
8835281501	PLT	Pilot Fish (Naucrates Ductor)
8835282401	LEE	Leer Fish (Lichia Amia)
8835330101	CAM	NA (Caristius Macropus)
8835430000	SBZ	Sea Breams (Sparidae)
8835430601	RPG	Redtail Porgy (Pagrus Pagrus)
8835430801	SBR	Red Seabream (Pagellus Bogaraveo)
8835430802	SBA	Auxillary Seabream (Pagellus Acarne)
8835430804	PAC	Pandora (Pagellus Erythrinus)
8835430901	BOG	Bogue (Boops Boops)
8835431000	DEX	NA (Dentex Spp)
8835431002	DEL	Large-Eyed Dentex (Dentex Macropthalmus)
8835431005	DEC	Dentex (Dentex Dentex)
8835431101	SBG	Gilt-Head Seabream (Sparus Auratus)
8835431102	SBC	Couch'S Seabream (Sparus Pagurus)
8835431201	BKS	Black Seabream (Spondyliosoma Cantharus)
8835440000	SCW	NA (Sciaenidae)
8835441107	UAC	NA (Umbrina Canariensis)
8835441108	UCR	NA (Umbrina Cirrosa)
8835442701	MGR	Meagre (Argyrosomus Regium)
8835450000	MLX	Goatfishes (Red Mullets) (Mullidae)
8835450200	MUX	Mullets (Red) (Mullus Spp)
8835450202	MUR	Red Mullet (Mullus Surmuletus)
8835450203	MBB	Red Mullet (Mullus Barbatus)
8835700000	CEY	NA (Cepolidae)
8835700102	RPF	Red Bandfish (Cepola Rubescens)
8835710000	BMX	NA (Bramidae)
8835710102	POA	Rays Bream (Pomfret) (Brama Brama)
8835710301	PYB	NA (Pterycombus Brama)
8835710403	TAP	NA (Taractes Asper)
8835710701	TAL	NA (Taractichthys Longipinnis)
8835750000	BSM	NA (Moronidae)
8835750100	BSE	Basses (Dicentrarchus (Morone) Spp)
8835750101	ESB	European Seabass (Dicentrarchus (Morone) Labrax)
8835750102	DMP	NA (Dicentrarchus (Morone) Punctatus)
8835750200	BSA	Basses (Morone Spp)
8835750202	STB	Striped Bass (Morone Saxatilis)
8836010000	MUL	Grey Mullets (Mugilidae)
8836010101	MTG	Grey Mullet (Mugil Cephalus)
8836010704	MTL	Thick Lipped Mullet (Chelon (Crenimugil) Labrosus)
8836010901	MTN	Thin Lipped Mullet (Liza Ramada)
8836010902	MGN	Golden Mullet (Liza Aurata)
8839010000	WRA	Wrasses (Labridae)
8839012306	RBW	Rainbow Wrasse (Coris Julis)
8839013301	CWG	Corkwing (Crenilabrus Melops)
8839013401	SMW	Small-Mouthed Wrasse (Centrolabrus Exoletus)
8839013501	GDY	Goldsinny (Ctenolabrus Rupestris)
8839013603	BNW	Ballan Wrasse (Labrus Bergylta)
8839013605	CUW	Cuckoo Wrasse (Labrus Mixtus)
8839013701	SRW	Scale-Rayed Wrasse (Acantholabrus Palloni)
8840060101	WEL	Lesser Weever Fish (Trachinus (Echiichthys) Vipera)
8840060102	WEG	Greater Weever Fish (Trachinus Draco)
8842010000	BNX	Blennies (Blenniidae)
8842010104	BBY	Butterfly Blenny (Blennius Ocellaris)
8842010110	TBY	Tompot Blenny (Blennius(Parablennius)Gattorugine)
8842010115	SHY	Shanny (Blennius (Lipophrys) Pholis)
8842012401	MBY	Montague'S Blenny (Coryphoblennius Galerita)
8842020000	CAZ	Rockfishes (Anarhichadidae)
8842020100	CAT	Catfishes (Anarhichas Spp)
8842020102	CAJ	Jelly Catfish (Anarhichas Denticulatus(Latifrons))
8842020103	CAA	Catfish (Wolffish) (Anarhichas Lupus)
8842020104	CAS	Spotted Catfish (Anarhichas Minor)
8842120000	SHZ	NA (Stichaeidae)
8842120505	YBY	Yarrel'S Blenny (Chirolophis Ascanii)
8842120905	SBY	Snake Blenny (Lumpenus Lampretaeformis)
8842121801	SNB	Spotted Snake Blenny (Leptoclinus Maculatus)
8842130209	BTF	Butter Fish (Pholis Gunnellus)
8845010000	SAX	Sandeels (Ammodytidae)
8845010100	SAN	Sandeels (Ammodytes Spp)
8845010105	TSE	Sandeel (Ammodytes Tobianus)
8845010106	MSE	Sandeel (Ammodytes Marinus)
8845010201	SMS	Smooth Sandeel (Gymnammodytes Semisquamatus)
8845010301	GSE	Great Sandeel (Hyperoplus Lanceeolatus)
8845010302	ISE	Immaculate Sandeel (Hyperoplus Immaculatus)
8846010000	DTX	Dragonets (Callionymidae)
8846010106	CDT	Common Dragonet (Callionymus Lyra)
8846010107	SDT	Spotted Dragonet (Callionymus Maculatus)
8846010120	RDT	Reticulate Dragonet (Callionymus Reticulatus)
8847000000	FGX	Gobies (Gobioidei)
8847010000	GPA	Gobies (Gobiidae)
8847011300	GOB	Gobies (Gobius Spp)
8847011304	GNG	Golden Goby (Gobius Auratus)
8847011307	GTG	Giant Goby (Gobius Cobitis)
8847011308	GBC	NA (Gobius Cruentatus)
8847011316	BLG	Black Goby (Gobius Niger)
8847011320	RKG	Rock Goby (Gobius Paganellus)
8847011325	GSV	Steven'S Goby (Gobius Gasteveni)
8847014901	CLG	Crystal Goby (Crystallogobius Linearis)
8847015001	TSG	Two-Spot Goby (Gobiusculus (Chaparrudo) Flavescens)
8847015101	SDG	Sand Goby (Pomatoschistus Minutus)
8847015102	PTG	Painted Goby (Pomatoschistus Pictus)
8847015103	GMG	Common Goby (Pomatoschistus Microps)
8847015104	NVG	Norwegian Goby (Pomatoschistus Norvegicus)
8847016501	GOD	Diminutive Goby (Lebetus Scorpioides (=L.Orca))
8847016502	GOG	Guillet'S Goby (Lebetus Guilleti)
8847016601	TPG	Transparent Goby (Aphia Minuta)
8847016702	FSG	Frie'S Goby (Lesueurigobius Friesii)
8847016802	JYG	Jeffrey'S Goby (Buenia Jeffreysii)
8847016901	LSG	Leopard Spotted Goby (Thorogobius Ephippiatus)
8850000000	MKX	NA (Scombroidei)
8850010000	ESX	Escolars (Gemplydae)
8850010401	ESR	Escolar (Ruvettus Pretiosus)
8850010701	NAN	NA (Nesiarchus Nasutus)
8850020000	CUT	Scabbard Fishes (Trichiuridae)
8850020101	SSF	Silvery Scabbard Fish (Benthodesmus Simonyi)
8850020201	HTL	Hair Tail (Trichiurus Lepturus)
8850020301	BSF	Black Scabbard Fish (Aphanopus Carbo)
8850020401	SFS	Cutlass Fish (Lepidopus Caudatus)
8850030000	MAX	Mackerels (Scombridae)
8850030101	SJT	Skipjack Tuna (Euthynnus Pelamis)
8850030105	LTU	Little Tuny (Euthynnus Quadripunctatus)
8850030202	BON	Bonito (Sarda Sarda)
8850030301	SPM	Spanish Mackerel (S.Japonicus)
8850030302	MAC	(European) Mackerel (Scomber Scombrus)
8850030401	ALB	Albacore (Thunnus Alalunga)
8850030402	BFT	Blue-Fin Tunny (Thunnus Thynnus)
8850030403	YEF	Yellow-Fin Tunny (Thunnus Albacares)
8850030405	BET	Big-Eye Tuna (Thunnus Obesus)
8850030701	FRM	Frigate Mackerel (Auxis Rochei)
8850030702	FRI	Frigate Mackerel (Auxis Thazard)
8850031201	PLB	Plain Bonito (Orcynopsis Unicolor)
8850040101	SWO	Swordfish (Xiphias Gladius)
8850050101	LUV	Luvar (Luvarus Imperialis)
8850060101	SAI	Sailfish (Istiophorus Platypterus (Albicans))
8850060301	WMN	White Marlin (Tetrapterus Albidus)
8851010201	BAF	Barrel Fish (Hyperoglyphe Perciforma)
8851010301	BKF	Blackfish (Centrolophus Niger)
8851010302	CBF	Cornish Blackfish (Centrolophus Medusophagus)
8851020203	CBG	NA (Cubiceps Gracilis)
8855000000	FLX	Flatfishes (Pleuronectiformes)
8857000000	FFX	Flatfishes (Pleuronectoidei)
8857030000	BTX	Left Eyed Flatfish (Bothidae)
8857030402	TUR	Turbot (Scophthalmus Maximus)
8857030403	BLL	Brill (Scophthalmus Rhombus)
8857031702	SDF	Scald Fish (Arnoglossus Laterna)
8857031703	ISF	Imperial Scaldfish (Arnoglossus Imperialis)
8857031706	ART	Spotted Scaldfish (Arnoglossus Thori)
8857032101	TKT	Topknot (Zeugopterus Punctatus)
8857032201	NKT	Norwegian Topknot (Phrynorhombus Norvegius)
8857032202	EKT	Ekstroms Topknot (Phrynorhombus Regius)
8857032300	LEX	NA (Lepidorhombus Spp)
8857032301	LBI	Four Spot Megrim (Lepidorhombus Boscii)
8857032302	MEG	Megrim (Lepidorhombus Whiffiagonis)
8857040000	PNX	Right Eyed Flatfish (Pleuronectidae)
8857040502	WIT	Witch (Glyptocephalus Cynoglossus)
8857040603	PLA	American Plaice (Lr Dab) (Hippoglossoides Platessoides)
8857040904	DAB	Dab (Limanda Limanda)
8857041202	LEM	Lemon Sole (Microstomus Kitt)
8857041402	FLE	Flounder (European) (Platichthys Flesus)
8857041502	PLE	European Plaice (Pleuronectes Platessa)
8857041801	GLH	Greenland Halibut (Reinhardtius Hippoglossoides)
8857041902	HAL	Halibut (Hippoglossus Hippoglossus)
8858010000	SOX	Soles (Soleidae)
8858010601	SOL	Sole (Dover Sole) (Solea Solea (S.Vulgaris))
8858010610	SOS	Sand Sole (Pegusa (Solea) Lascaris)
8858010801	SOT	Solenette (Buglossidium Luteum)
8858010902	DGA	NA (Microchirus (Dicologlossa) Azevia)
8858010903	TBS	Thickback Sole (Microchirus Variegatus)
8858012100	CED	NA (Dicologlossa)
8858012101	BYP	NA (Bathysolea Profundicola)
8858020000	TOX	Tongue Soles (Cynoglossidae)
8858020201	TES	Tongue Sole (Cynoglossus Browni)
8860020200	TRI	Trigger Fish (Balistes Spp)
8860020205	TRF	Trigger Fish (Balistes Carolinensis)
8860020501	CDM	NA (Canthidermis Maculatus)
8861010102	PFF	Puffer Fish (Lagocephalus Lagocephalus)
8861040101	SUN	Sunfish (Mola Mola)
8861040201	TSU	Truncated Sunfish (Ranzania Laevis)
9200000000	MAM	Aquatic Mammels (Mammalia)
9218000000	ODN	Toothed Whales (Nei) (Cetacea-Odontoceti)
9218020000	DLP	Dolphins (Delphinidae)
9218020401	DBO	Bottlenose Dolphin (Tursiops Truncatus)
9218020500	DSP	Spotted Dolphins (Stenella Spp)
9218020504	DST	Striped Dolphin (Stenella Coeruleoalba)
9218020536	DEN	Euphosyne Dolphin (Stenella Styx)
9218020601	DCO	Common Dolphin (Delphinus Delphis)
9218020801	DWB	White-Beaked Dolphin (Lagenorhynchus Albirostris)
9218020802	DWS	White-Sided Dolphin (Lagenorhynchus Acutus)
9218021101	DDR	Risso's Dolphin (Grampus Griseus)
9218021401	FKW	False Killer Whale (Pseudorca Crassidens)
9218021501	PIW	L.F.Pilot Whale (Globicephala Melaena)
9218021601	KIW	Killer Whale (Orcinus Orca)
9219020000	BAX	NA (Balaenopteridae)
9999999999	TBX	Tube Worms (Tubeworms)
\.


--
-- Data for Name: stomach_state; Type: TABLE DATA; Schema: public; Owner: lentinj
--

COPY stomach_state (stomach_state_id, name, description) FROM stdin;
1	Empty	
2	Full	
3	Regurgitated	
4	Remains	Only skeletal remains
5	Inverted	Stomach inverted
99	Lost	
\.


--
-- Data for Name: survey_index; Type: TABLE DATA; Schema: public; Owner: lentinj
--

COPY survey_index (survey_index_id, data_source_id, case_study_id, index_type_id, areacell_id, year, month, value) FROM stdin;
\.


--
-- Name: survey_index_survey_index_id_seq; Type: SEQUENCE SET; Schema: public; Owner: lentinj
--

SELECT pg_catalog.setval('survey_index_survey_index_id_seq', 1, false);


--
-- Data for Name: vessel; Type: TABLE DATA; Schema: public; Owner: lentinj
--

COPY vessel (vessel_id, name, description) FROM stdin;
101	1	<12m
1011	1.RSH	<12m research
1012	1.COM	<12m commercial
1013	1.CQT	<12m commercial, quota
1014	1.CDA	<12m commercial, days
1015	1.FGN	<12m foreign
1016	1.FRZ	<12m freezer
102	2	12-24m
1021	2.RSH	12-24m research
1022	2.COM	12-24m commercial
1023	2.CQT	12-24m commercial, quota
1024	2.CDA	12-24m commercial, days
1025	2.FGN	12-24m foreign
1026	2.FRZ	12-24m freezer
103	3	>24m
1031	3.RSH	>24m research
1032	3.COM	>24m commercial
1033	3.CQT	>24m commercial, quota
1034	3.CDA	>24m commercial, days
1035	3.FGN	>24m foreign
1036	3.FRZ	>24m freezer
\.


--
-- Name: areacell_case_study_id_name_key; Type: CONSTRAINT; Schema: public; Owner: lentinj; Tablespace: 
--

ALTER TABLE ONLY areacell
    ADD CONSTRAINT areacell_case_study_id_name_key UNIQUE (case_study_id, name);


--
-- Name: areacell_pkey; Type: CONSTRAINT; Schema: public; Owner: lentinj; Tablespace: 
--

ALTER TABLE ONLY areacell
    ADD CONSTRAINT areacell_pkey PRIMARY KEY (case_study_id, areacell_id);


--
-- Name: case_study_name_key; Type: CONSTRAINT; Schema: public; Owner: lentinj; Tablespace: 
--

ALTER TABLE ONLY case_study
    ADD CONSTRAINT case_study_name_key UNIQUE (name);


--
-- Name: case_study_pkey; Type: CONSTRAINT; Schema: public; Owner: lentinj; Tablespace: 
--

ALTER TABLE ONLY case_study
    ADD CONSTRAINT case_study_pkey PRIMARY KEY (case_study_id);


--
-- Name: data_source_case_study_id_name_key; Type: CONSTRAINT; Schema: public; Owner: lentinj; Tablespace: 
--

ALTER TABLE ONLY data_source
    ADD CONSTRAINT data_source_case_study_id_name_key UNIQUE (case_study_id, name);


--
-- Name: data_source_pkey; Type: CONSTRAINT; Schema: public; Owner: lentinj; Tablespace: 
--

ALTER TABLE ONLY data_source
    ADD CONSTRAINT data_source_pkey PRIMARY KEY (case_study_id, data_source_id);


--
-- Name: digestion_stage_name_key; Type: CONSTRAINT; Schema: public; Owner: lentinj; Tablespace: 
--

ALTER TABLE ONLY digestion_stage
    ADD CONSTRAINT digestion_stage_name_key UNIQUE (name);


--
-- Name: digestion_stage_pkey; Type: CONSTRAINT; Schema: public; Owner: lentinj; Tablespace: 
--

ALTER TABLE ONLY digestion_stage
    ADD CONSTRAINT digestion_stage_pkey PRIMARY KEY (digestion_stage_id);


--
-- Name: division_pkey; Type: CONSTRAINT; Schema: public; Owner: lentinj; Tablespace: 
--

ALTER TABLE ONLY division
    ADD CONSTRAINT division_pkey PRIMARY KEY (division_id);


--
-- Name: fleet_name_key; Type: CONSTRAINT; Schema: public; Owner: lentinj; Tablespace: 
--

ALTER TABLE ONLY fleet
    ADD CONSTRAINT fleet_name_key UNIQUE (name);


--
-- Name: fleet_pkey; Type: CONSTRAINT; Schema: public; Owner: lentinj; Tablespace: 
--

ALTER TABLE ONLY fleet
    ADD CONSTRAINT fleet_pkey PRIMARY KEY (fleet_id);


--
-- Name: gear_name_key; Type: CONSTRAINT; Schema: public; Owner: lentinj; Tablespace: 
--

ALTER TABLE ONLY gear
    ADD CONSTRAINT gear_name_key UNIQUE (name);


--
-- Name: gear_pkey; Type: CONSTRAINT; Schema: public; Owner: lentinj; Tablespace: 
--

ALTER TABLE ONLY gear
    ADD CONSTRAINT gear_pkey PRIMARY KEY (gear_id);


--
-- Name: index_type_case_study_id_name_key; Type: CONSTRAINT; Schema: public; Owner: lentinj; Tablespace: 
--

ALTER TABLE ONLY index_type
    ADD CONSTRAINT index_type_case_study_id_name_key UNIQUE (case_study_id, name);


--
-- Name: index_type_pkey; Type: CONSTRAINT; Schema: public; Owner: lentinj; Tablespace: 
--

ALTER TABLE ONLY index_type
    ADD CONSTRAINT index_type_pkey PRIMARY KEY (case_study_id, index_type_id);


--
-- Name: institute_name_key; Type: CONSTRAINT; Schema: public; Owner: lentinj; Tablespace: 
--

ALTER TABLE ONLY institute
    ADD CONSTRAINT institute_name_key UNIQUE (name);


--
-- Name: institute_pkey; Type: CONSTRAINT; Schema: public; Owner: lentinj; Tablespace: 
--

ALTER TABLE ONLY institute
    ADD CONSTRAINT institute_pkey PRIMARY KEY (institute_id);


--
-- Name: market_category_name_key; Type: CONSTRAINT; Schema: public; Owner: lentinj; Tablespace: 
--

ALTER TABLE ONLY market_category
    ADD CONSTRAINT market_category_name_key UNIQUE (name);


--
-- Name: market_category_pkey; Type: CONSTRAINT; Schema: public; Owner: lentinj; Tablespace: 
--

ALTER TABLE ONLY market_category
    ADD CONSTRAINT market_category_pkey PRIMARY KEY (market_category_id);


--
-- Name: maturity_stage_name_key; Type: CONSTRAINT; Schema: public; Owner: lentinj; Tablespace: 
--

ALTER TABLE ONLY maturity_stage
    ADD CONSTRAINT maturity_stage_name_key UNIQUE (name);


--
-- Name: maturity_stage_pkey; Type: CONSTRAINT; Schema: public; Owner: lentinj; Tablespace: 
--

ALTER TABLE ONLY maturity_stage
    ADD CONSTRAINT maturity_stage_pkey PRIMARY KEY (maturity_stage_id);


--
-- Name: predator_pkey; Type: CONSTRAINT; Schema: public; Owner: lentinj; Tablespace: 
--

ALTER TABLE ONLY predator
    ADD CONSTRAINT predator_pkey PRIMARY KEY (predator_id);


--
-- Name: prey_pkey; Type: CONSTRAINT; Schema: public; Owner: lentinj; Tablespace: 
--

ALTER TABLE ONLY prey
    ADD CONSTRAINT prey_pkey PRIMARY KEY (prey_id);


--
-- Name: sample_pkey; Type: CONSTRAINT; Schema: public; Owner: lentinj; Tablespace: 
--

ALTER TABLE ONLY sample
    ADD CONSTRAINT sample_pkey PRIMARY KEY (sample_id);


--
-- Name: sampling_type_case_study_id_name_key; Type: CONSTRAINT; Schema: public; Owner: lentinj; Tablespace: 
--

ALTER TABLE ONLY sampling_type
    ADD CONSTRAINT sampling_type_case_study_id_name_key UNIQUE (case_study_id, name);


--
-- Name: sampling_type_pkey; Type: CONSTRAINT; Schema: public; Owner: lentinj; Tablespace: 
--

ALTER TABLE ONLY sampling_type
    ADD CONSTRAINT sampling_type_pkey PRIMARY KEY (case_study_id, sampling_type_id);


--
-- Name: sex_name_key; Type: CONSTRAINT; Schema: public; Owner: lentinj; Tablespace: 
--

ALTER TABLE ONLY sex
    ADD CONSTRAINT sex_name_key UNIQUE (name);


--
-- Name: sex_pkey; Type: CONSTRAINT; Schema: public; Owner: lentinj; Tablespace: 
--

ALTER TABLE ONLY sex
    ADD CONSTRAINT sex_pkey PRIMARY KEY (sex_id);


--
-- Name: species_name_key; Type: CONSTRAINT; Schema: public; Owner: lentinj; Tablespace: 
--

ALTER TABLE ONLY species
    ADD CONSTRAINT species_name_key UNIQUE (name);


--
-- Name: species_pkey; Type: CONSTRAINT; Schema: public; Owner: lentinj; Tablespace: 
--

ALTER TABLE ONLY species
    ADD CONSTRAINT species_pkey PRIMARY KEY (species_id);


--
-- Name: stomach_state_name_key; Type: CONSTRAINT; Schema: public; Owner: lentinj; Tablespace: 
--

ALTER TABLE ONLY stomach_state
    ADD CONSTRAINT stomach_state_name_key UNIQUE (name);


--
-- Name: stomach_state_pkey; Type: CONSTRAINT; Schema: public; Owner: lentinj; Tablespace: 
--

ALTER TABLE ONLY stomach_state
    ADD CONSTRAINT stomach_state_pkey PRIMARY KEY (stomach_state_id);


--
-- Name: survey_index_pkey; Type: CONSTRAINT; Schema: public; Owner: lentinj; Tablespace: 
--

ALTER TABLE ONLY survey_index
    ADD CONSTRAINT survey_index_pkey PRIMARY KEY (survey_index_id);


--
-- Name: vessel_name_key; Type: CONSTRAINT; Schema: public; Owner: lentinj; Tablespace: 
--

ALTER TABLE ONLY vessel
    ADD CONSTRAINT vessel_name_key UNIQUE (name);


--
-- Name: vessel_pkey; Type: CONSTRAINT; Schema: public; Owner: lentinj; Tablespace: 
--

ALTER TABLE ONLY vessel
    ADD CONSTRAINT vessel_pkey PRIMARY KEY (vessel_id);


--
-- Name: areacell_case_study_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: lentinj
--

ALTER TABLE ONLY areacell
    ADD CONSTRAINT areacell_case_study_id_fkey FOREIGN KEY (case_study_id) REFERENCES case_study(case_study_id);


--
-- Name: data_source_case_study_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: lentinj
--

ALTER TABLE ONLY data_source
    ADD CONSTRAINT data_source_case_study_id_fkey FOREIGN KEY (case_study_id) REFERENCES case_study(case_study_id);


--
-- Name: division_case_study_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: lentinj
--

ALTER TABLE ONLY division
    ADD CONSTRAINT division_case_study_id_fkey FOREIGN KEY (case_study_id) REFERENCES case_study(case_study_id);


--
-- Name: division_case_study_id_fkey1; Type: FK CONSTRAINT; Schema: public; Owner: lentinj
--

ALTER TABLE ONLY division
    ADD CONSTRAINT division_case_study_id_fkey1 FOREIGN KEY (case_study_id, areacell_id) REFERENCES areacell(case_study_id, areacell_id);


--
-- Name: index_type_case_study_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: lentinj
--

ALTER TABLE ONLY index_type
    ADD CONSTRAINT index_type_case_study_id_fkey FOREIGN KEY (case_study_id) REFERENCES case_study(case_study_id);


--
-- Name: predator_case_study_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: lentinj
--

ALTER TABLE ONLY predator
    ADD CONSTRAINT predator_case_study_id_fkey FOREIGN KEY (case_study_id) REFERENCES case_study(case_study_id);


--
-- Name: predator_case_study_id_fkey1; Type: FK CONSTRAINT; Schema: public; Owner: lentinj
--

ALTER TABLE ONLY predator
    ADD CONSTRAINT predator_case_study_id_fkey1 FOREIGN KEY (case_study_id, data_source_id) REFERENCES data_source(case_study_id, data_source_id);


--
-- Name: predator_case_study_id_fkey2; Type: FK CONSTRAINT; Schema: public; Owner: lentinj
--

ALTER TABLE ONLY predator
    ADD CONSTRAINT predator_case_study_id_fkey2 FOREIGN KEY (case_study_id, areacell_id) REFERENCES areacell(case_study_id, areacell_id);


--
-- Name: predator_case_study_id_fkey3; Type: FK CONSTRAINT; Schema: public; Owner: lentinj
--

ALTER TABLE ONLY predator
    ADD CONSTRAINT predator_case_study_id_fkey3 FOREIGN KEY (case_study_id, sampling_type_id) REFERENCES sampling_type(case_study_id, sampling_type_id);


--
-- Name: predator_gear_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: lentinj
--

ALTER TABLE ONLY predator
    ADD CONSTRAINT predator_gear_id_fkey FOREIGN KEY (gear_id) REFERENCES gear(gear_id);


--
-- Name: predator_institute_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: lentinj
--

ALTER TABLE ONLY predator
    ADD CONSTRAINT predator_institute_id_fkey FOREIGN KEY (institute_id) REFERENCES institute(institute_id);


--
-- Name: predator_maturity_stage_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: lentinj
--

ALTER TABLE ONLY predator
    ADD CONSTRAINT predator_maturity_stage_id_fkey FOREIGN KEY (maturity_stage_id) REFERENCES maturity_stage(maturity_stage_id);


--
-- Name: predator_sex_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: lentinj
--

ALTER TABLE ONLY predator
    ADD CONSTRAINT predator_sex_id_fkey FOREIGN KEY (sex_id) REFERENCES sex(sex_id);


--
-- Name: predator_species_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: lentinj
--

ALTER TABLE ONLY predator
    ADD CONSTRAINT predator_species_id_fkey FOREIGN KEY (species_id) REFERENCES species(species_id);


--
-- Name: predator_stomach_state_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: lentinj
--

ALTER TABLE ONLY predator
    ADD CONSTRAINT predator_stomach_state_id_fkey FOREIGN KEY (stomach_state_id) REFERENCES stomach_state(stomach_state_id);


--
-- Name: predator_vessel_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: lentinj
--

ALTER TABLE ONLY predator
    ADD CONSTRAINT predator_vessel_id_fkey FOREIGN KEY (vessel_id) REFERENCES vessel(vessel_id);


--
-- Name: prey_digestion_stage_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: lentinj
--

ALTER TABLE ONLY prey
    ADD CONSTRAINT prey_digestion_stage_id_fkey FOREIGN KEY (digestion_stage_id) REFERENCES digestion_stage(digestion_stage_id);


--
-- Name: prey_predator_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: lentinj
--

ALTER TABLE ONLY prey
    ADD CONSTRAINT prey_predator_id_fkey FOREIGN KEY (predator_id) REFERENCES predator(predator_id);


--
-- Name: prey_species_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: lentinj
--

ALTER TABLE ONLY prey
    ADD CONSTRAINT prey_species_id_fkey FOREIGN KEY (species_id) REFERENCES species(species_id);


--
-- Name: sample_case_study_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: lentinj
--

ALTER TABLE ONLY sample
    ADD CONSTRAINT sample_case_study_id_fkey FOREIGN KEY (case_study_id) REFERENCES case_study(case_study_id);


--
-- Name: sample_case_study_id_fkey1; Type: FK CONSTRAINT; Schema: public; Owner: lentinj
--

ALTER TABLE ONLY sample
    ADD CONSTRAINT sample_case_study_id_fkey1 FOREIGN KEY (case_study_id, data_source_id) REFERENCES data_source(case_study_id, data_source_id);


--
-- Name: sample_case_study_id_fkey2; Type: FK CONSTRAINT; Schema: public; Owner: lentinj
--

ALTER TABLE ONLY sample
    ADD CONSTRAINT sample_case_study_id_fkey2 FOREIGN KEY (case_study_id, areacell_id) REFERENCES areacell(case_study_id, areacell_id);


--
-- Name: sample_case_study_id_fkey3; Type: FK CONSTRAINT; Schema: public; Owner: lentinj
--

ALTER TABLE ONLY sample
    ADD CONSTRAINT sample_case_study_id_fkey3 FOREIGN KEY (case_study_id, sampling_type_id) REFERENCES sampling_type(case_study_id, sampling_type_id);


--
-- Name: sample_gear_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: lentinj
--

ALTER TABLE ONLY sample
    ADD CONSTRAINT sample_gear_id_fkey FOREIGN KEY (gear_id) REFERENCES gear(gear_id);


--
-- Name: sample_institute_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: lentinj
--

ALTER TABLE ONLY sample
    ADD CONSTRAINT sample_institute_id_fkey FOREIGN KEY (institute_id) REFERENCES institute(institute_id);


--
-- Name: sample_maturity_stage_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: lentinj
--

ALTER TABLE ONLY sample
    ADD CONSTRAINT sample_maturity_stage_id_fkey FOREIGN KEY (maturity_stage_id) REFERENCES maturity_stage(maturity_stage_id);


--
-- Name: sample_sex_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: lentinj
--

ALTER TABLE ONLY sample
    ADD CONSTRAINT sample_sex_id_fkey FOREIGN KEY (sex_id) REFERENCES sex(sex_id);


--
-- Name: sample_species_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: lentinj
--

ALTER TABLE ONLY sample
    ADD CONSTRAINT sample_species_id_fkey FOREIGN KEY (species_id) REFERENCES species(species_id);


--
-- Name: sample_vessel_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: lentinj
--

ALTER TABLE ONLY sample
    ADD CONSTRAINT sample_vessel_id_fkey FOREIGN KEY (vessel_id) REFERENCES vessel(vessel_id);


--
-- Name: sampling_type_case_study_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: lentinj
--

ALTER TABLE ONLY sampling_type
    ADD CONSTRAINT sampling_type_case_study_id_fkey FOREIGN KEY (case_study_id) REFERENCES case_study(case_study_id);


--
-- Name: survey_index_case_study_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: lentinj
--

ALTER TABLE ONLY survey_index
    ADD CONSTRAINT survey_index_case_study_id_fkey FOREIGN KEY (case_study_id) REFERENCES case_study(case_study_id);


--
-- Name: survey_index_case_study_id_fkey1; Type: FK CONSTRAINT; Schema: public; Owner: lentinj
--

ALTER TABLE ONLY survey_index
    ADD CONSTRAINT survey_index_case_study_id_fkey1 FOREIGN KEY (case_study_id, data_source_id) REFERENCES data_source(case_study_id, data_source_id);


--
-- Name: survey_index_case_study_id_fkey2; Type: FK CONSTRAINT; Schema: public; Owner: lentinj
--

ALTER TABLE ONLY survey_index
    ADD CONSTRAINT survey_index_case_study_id_fkey2 FOREIGN KEY (case_study_id, index_type_id) REFERENCES index_type(case_study_id, index_type_id);


--
-- Name: survey_index_case_study_id_fkey3; Type: FK CONSTRAINT; Schema: public; Owner: lentinj
--

ALTER TABLE ONLY survey_index
    ADD CONSTRAINT survey_index_case_study_id_fkey3 FOREIGN KEY (case_study_id, areacell_id) REFERENCES areacell(case_study_id, areacell_id);


--
-- Name: public; Type: ACL; Schema: -; Owner: lentinj
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM lentinj;
GRANT ALL ON SCHEMA public TO lentinj;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--

