#! /bin/bash

# create upload directories
#rm -rf uploadDir
#rm -rf uploadDirCustom
#mkdir uploadDir
#mkdir uploadDirCustom

# create log directory
#rm -rf logsDir
#mkdir logsDir

# create database dir and the database
#rm -rf databaseDir
#mkdir databaseDir
#sqlite3 ./databaseDir/imageDatabase.db <<SQL_ENTRY_TAG_1
#CREATE TABLE images (number INTEGER PRIMARY KEY, name VARCHAR(40), resizedto VARCHAR(40), ipaddress VARCHAR(20), time VARCHAR(40));
#SQL_ENTRY_TAG_1

# populate database
#sqlite3 ./databaseDir/imageDatabase.db <<SQL_ENTRY_TAG_2
#INSERT INTO images VALUES (null, "abcdefghijklmnopqrst.jpg", "192x192!", "127.0.0.1" , "2011-08-26 11:43:53.135684");
#INSERT INTO images VALUES (null, "abcdefghijklmnopqrst.jpg", "192x192!", "127.0.0.1" , "2011-08-26 11:43:53.135684");
#INSERT INTO images VALUES (null, "abcdefghijklmnopqrst.jpg", "192x192!", "127.0.0.1" , "2011-08-26 11:43:53.135684");
#INSERT INTO images VALUES (null, "abcdefghijklmnopqrst.jpg", "192x192!", "127.0.0.1" , "2011-08-26 11:43:53.135684");
#INSERT INTO images VALUES (null, "abcdefghijklmnopqrst.jpg", "192x192!", "127.0.0.1" , "2011-08-26 11:43:53.135684");
#INSERT INTO images VALUES (null, "abcdefghijklmnopqrst.jpg", "192x192!", "127.0.0.1" , "2011-08-26 11:43:53.135684");
#SQL_ENTRY_TAG_2
#

# compile everything
ghc --make CommonStuff.hs &&
ghc --make -o index.fcgi codeIndex.hs &&
ghc --make -o resize.fcgi codeResize.hs &&
ghc --make -o upload.fcgi codeUpload.hs &&
ghc --make -o view.fcgi codeView.hs


