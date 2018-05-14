# Delectus 2 Data in SQLite

This document describes how the data objects described in "data" are
realized in a SQLite document.

## User Objects

### Fields
### Rows
### Columns
### Lists
### Collections

## Platform Objects

### Containers

In the SQLite implementation, a **Container** is a SQLite file. It
contains a set of Collections and Lists, plus some tables of metadata
that enable the Curator to correctly manage the contents of the file.

### Repositories

A **Repository** is a set of SQLite Containers, plus a SQLite file
that records the ownership of each List and Collection, and identifies
which Container contains each one. 

### Curators


