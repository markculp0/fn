
***

# fn

***

Some R wrapper/utility functions for quick command line use.

* **col2txt** - Convert a data frame column to text.  Provide column number to dump.

```
    fn::col2txt(df, 3)
```

* **desc_cnt** - Descending order count of a data frame column.

```
    fn::desc_cnt(df, "EventID")
```

* **desc_cnt2** - Descending order count of two data frame columns.

```
    fn::desc_cnt2(df, "EventID", "Type")
```

* **doc2txt** - Convert a Word .docx file to text

```
    fn::doc2txt("file.docx")
```

* **dt_range** - Display the date range of a categorical column in a data frame, given date column and categorical column.

```
    fn::dt_range(df, "TimeGenerated", "EventID")
```

* **dt_range2** - Display the date range of two categorical columns in a data frame.

```
    fn::dt_range(df, "TimeGenerated", "EventID", "Type")
```

* **dttm_mdy_hm** - Add a properly formatted datetime stamp to beginning of a log file in month/day/year hour/minute format. 

```
    df2 <- dttm_mdy_hm(df, df$DateTime)
```

* **dttm_mdy_hms** - Add a properly formatted datetime stamp to beginning of a log file in month/day/year hour/minute/second format. 

```
    df2 <- dttm_mdy_hms(df, df$DateTime)
```

* **evt_id_join** - Join descriptions of Windows Security log EventIDs to a data frame.  Assumes a data frame with an EventID column.

```
    fn::evt_id_join(df)
```

* **gplot_by_day** - Plot time series data in a bar chart by date.

```
    fn::gplot_by_day(df, df$TimeGenerated)    
```

* **gplot_by_hour** - Plot time series data in a bar chart by hour of a given date.

```
    fn::gplot_by_hour(df, df$TimeGenerated, "2022-06-22")
```

* **rautoruns** - Read a tab separated Sysinternals autoruns output file and return dataframes of interest.  

```
    fn::rautoruns("autoruns.csv")
```

* **rcports** - Read a tab separated, NirSoft CurrPorts output file and return dataframes of interest.  

```
    fn::rcports("cports.csv")
```

* **rcsv** - Read a CSV file into a data frame.  

```
    df <- fn::rcsv("evtlog.csv")
```

* **rcsvn** - Read a CSV file with no headers.  

```
    df <- fn::rcsvn("evtlog.csv")
```

* **rtsv** - Read a tab separated file into a data frame.  

```
    df <- fn::rtsv("evtlog.tsv")
```

* **rxl** - Read an Excel file into a data frame.

```
    df <- fn::rxl("file.xlsx")
```

* **wcsv** - Write a data frame to a CSV file.

```
    fn::wcsv(df, "file.csv")
```

* **wheq** - Where equal to filter.

```
    df2 <- fn::wheq(df, "EventID", "4624")
```

* **wevt_logon_4624** - Get Windows event ID 4624 logon success from Security.evtx.

```
    df <- wevt_logon_4624(20)
```

* **wevt_logoni_4624** - Get Windows Interactive, event ID 4624 logon success from Security.evtx.

```
    df <- wevt_logoni_4624(20)
```

* **wevt_logonfail_4625** - Get Windows event ID 4625 logon failures from Security.evtx.

```
    df <- wevt_logonfail_4625(5)
```

* **wevt_logoff_4634** - Get Windows event ID 4634 logoff from Security.evtx.

```
    df <- wevt_logoff_4634(5)
```

* **wevt_logoff_4647** - Get Windows event ID 4647 user initiated logoff from Security.evtx.

```
    df <- wevt_logoff_4647(5)
```

* **whgt** - Where greater than or equal to filter. 

```
    df2 <- fn::whgt(df, "TimeGenerated", "2022-06-30")
```

* **whlt** - Where less than or equal to filter.

```
    df2 <- fn::whlt(df, "TimeGenerated", "2022-06-30")
```

* **wxl** - Write data frame to an Excel file

```
    fn::wxl(df, "file.xlsx")
```


***
***
