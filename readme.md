
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

* **dt_range** - Display the date range of a categorical column in a data frame, given date column and categorical column.

```
    fn::dt_range(df, "TimeGenerated", "EventID")
```

* **dt_range2** - Display the date range of two categorical columns in a data frame.

```
    fn::dt_range(df, "TimeGenerated", "EventID", "Type")
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
