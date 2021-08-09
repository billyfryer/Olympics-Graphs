# Olympics Graphs

During the 2020 Tokyo Olympic Games, I decided to make a graphic for every day starting with Day 0 (Opening Ceremonies). These include both graphs and tables made with the `ggplot2` package and the `GT` package respectively. I wanted to make the code for these public so that if anyone wanted to make a spinoff or learn some of the things that I learned while creating the graphs, it would be much easier. I have made it so that all code is reporducibe. This GitHub Repo is organized in the following way:

The **Data Sets** folder contains all 3 data sets used in these graphics. It is currently zipped so when downloaded, it needs to be unzipped. The *noc_regions.csv* and *athlete_events.csv* files are both from https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results/code?datasetId=31029&language=R and the *USA WBB Legacy.csv* is one that I created using Wikipedia. The only other data used is on Day 12. That data is from USA Today and I found it via Insider.com. That data is directly typed into the R Script for Day 12.

The **Flags and Icons** folder contains all flags and sport icons used in the graphics. The flags are from Wikipedia and the icons can be saved from https://www.theolympicdesign.com/olympic-design/pictograms/tokyo-2020/.

The **Outputs** folder contains outputs from all of the code created. These are labeled by Date and give a very brief description as to what each graphic is about.

Finally, the **Tokyo Olympucs 2020 Code** folder contains all the code from this series. The names follow a similar naming convention as the outputs for easy pairing. Almost every line is commented, but feel free to reach out if anything is confusing.

The last thing in this GitHub Repo is the **Olympics R Project**. This helps take care of file paths.
