#!/bin/sh

# Delete existing file
#rm main.tex

# Compile New File
cat ../01_introduction.Rmd ../02_photovoltaic_domain.Rmd \
	../03_methods.Rmd ../04_analysis.Rmd \
	../05_conclusion.Rmd > main.tex

# Replace \n# with \newpage \n \\section*{ ... }

# Replace \n## with \n \\subsection*{ ... }

# Replace <!-- .*?--> with nothing

# Remove \cite{NEEDED}

# Replace ``` ... ``` with \begin{lstlisting} ... \end{lstlisting}

# Replace ` ... ` with \listinline{ ... } 

# Underscores inline need to be escaped \_