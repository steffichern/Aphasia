## ------------------------------------------------------------------------------------------------
library(ggplot2)
cinderella <- read.csv("~/Downloads/36-490 S23/Cinderella.csv")
cat <- read.csv("~/Downloads/36-490 S23/Cat.csv")
sandwich <- read.csv("~/Downloads/36-490 S23/Sandwich.csv")
stroke <- read.csv("~/Downloads/36-490 S23/Stroke.csv")
illness <- read.csv("~/Downloads/36-490 S23/Illness.csv")
stroke_illness <- rbind(stroke, illness)


## ------------------------------------------------------------------------------------------------
# Ignoring groups that occur too few times
cinderella <- cinderella[cinderella$Group != "TransMotor", ]
cinderella <- cinderella[cinderella$Group != "TransSensory", ]
cinderella <- cinderella[cinderella$Group != "aphasia", ]
cinderella <- cinderella[cinderella$Group != "Global", ]
cat <- cat[cat$Group != "TransMotor", ]
cat <- cat[cat$Group != "TransSensory", ]
cat <- cat[cat$Group != "aphasia", ]
cat <- cat[cat$Group != "Global", ]
sandwich <- sandwich[sandwich$Group != "TransMotor", ]
sandwich <- sandwich[sandwich$Group != "TransSensory", ]
sandwich <- sandwich[sandwich$Group != "", ]
sandwich <- sandwich[sandwich$Group != "aphasia", ]
sandwich <- sandwich[sandwich$Group != "Global", ]
stroke <- stroke[stroke$Group != "TransMotor", ]
stroke <- stroke[stroke$Group != "TransSensory", ]
stroke <- stroke[stroke$Group != ".", ]
stroke <- stroke[stroke$Group != "aphasia", ]
stroke <- stroke[stroke$Group != "Global", ]
stroke_illness <- stroke_illness[stroke_illness$Group != "TransMotor", ]
stroke_illness <- stroke_illness[stroke_illness$Group != "TransSensory", ]
stroke_illness <- stroke_illness[stroke_illness$Group != ".", ]
stroke_illness <- stroke_illness[stroke_illness$Group != "aphasia", ]
stroke_illness <- stroke_illness[stroke_illness$Group != "Global", ]


## ------------------------------------------------------------------------------------------------
levels = c("control", "Anomic", "Broca", "Conduction", "NotAphasicByWAB", "Wernicke")

ggplot(cinderella, aes(x = factor(Group, levels = levels), y = X._Phrase_repetitions.1, fill = factor(Group, levels = levels))) + 
  geom_boxplot() +
  labs(title = "Distribution of % Phrase Repetitions Given Aphasia Group",
       y = "% # of Phrase Repetitions", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(cinderella, aes(x = factor(Group, levels = levels), y = log(X._Phrase_repetitions.1+0.01), fill = factor(Group, levels = levels))) + 
  geom_boxplot() +
  labs(title = "Distribution of % Phrase Repetitions Given Aphasia Group",
       y = "Log (% # of Phrase Repetitions)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(cinderella, aes(x = Group, y = sqrt(X._Phrase_repetitions.1), fill = factor(Group, levels = levels))) + 
  geom_boxplot() +
  labs(title = "Distribution of % Phrase Repetitions Given Aphasia Group",
       y = "Sqrt (% # of Phrase Repetitions)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## ------------------------------------------------------------------------------------------------
ggplot(cinderella, aes(x = Group, y = X._Word_revisions.1, fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Word Revisions Given Aphasia Group",
  y = "% # of Word Revisions", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(cinderella, aes(x = factor(Group, levels = levels), y = log(X._Word_revisions.1+0.01), 
                       fill = factor(Group, levels = levels))) + geom_boxplot() +
  labs(title = "Distribution of % Word Revisions Given Aphasia Group",
  y = "Log (% # of Word Revisions)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(cinderella, aes(x = Group, y = sqrt(X._Word_revisions.1), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Word Revisions Given Aphasia Group",
  y = "Sqrt (% # of Word Revisions)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## ------------------------------------------------------------------------------------------------
ggplot(cinderella, aes(x = Group, y = X._Phrase_revisions.1, fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of Phrase Revisions Given Aphasia Group",
  y = "% # of Phrase Revisions", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(cinderella, aes(x = factor(Group, levels = levels), y = log(X._Phrase_revisions.1+0.01), fill = factor(Group, levels = levels))) + geom_boxplot() +
  labs(title = "Distribution of Phrase Revisions Given Aphasia Group",
  y = "Log(% # of Phrase Revisions)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(cinderella, aes(x = Group, y = sqrt(X._Phrase_revisions.1), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Phrase Revisions Given Aphasia Group",
  y = "Sqrt(% # of Phrase Revisions)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## ------------------------------------------------------------------------------------------------
ggplot(cinderella, aes(x = Group, y = X._Phonological_fragment.1, fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Phonological Fragment Given Aphasia Group",
  y = "% # of Phonological Fragment", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(cinderella, aes(x = factor(Group, levels = levels), y = log(X._Phonological_fragment.1+0.01), fill = factor(Group, levels = levels))) + geom_boxplot() +
  labs(title = "Distribution of % Phonological Fragment Given Aphasia Group",
  y = "Log(% # of Phonological Fragment)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(cinderella, aes(x = Group, y = sqrt(X._Phonological_fragment.1), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Phonological Fragment Given Aphasia Group",
  y = "Sqrt(% # of Phonological Fragment)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## ------------------------------------------------------------------------------------------------
ggplot(cinderella, aes(x = Group, y = X._Filled_pauses.1, fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Filled Pauses Given Aphasia Group",
  y = "% # of Filled Pauses", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(cinderella, aes(x = factor(Group, levels = levels), y = log(X._Filled_pauses.1+0.01), fill = factor(Group, levels = levels))) + geom_boxplot() +
  labs(title = "Distribution of % Filled Pauses Given Aphasia Group",
  y = "Log(% # of Filled Pauses)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(cinderella, aes(x = Group, y = sqrt(X._Filled_pauses), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Filled Pauses Given Aphasia Group",
  y = "Sqrt(% # of Filled Pauses)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## ------------------------------------------------------------------------------------------------
ggplot(cinderella, aes(x = Group, y = X._WWR.1, fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Whole Word Repetition Given Aphasia Group",
  y = "% # of Whole Word Repetition", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(cinderella, aes(x = factor(Group, levels = levels), y = log(X._WWR.1+0.01), fill = factor(Group, levels = levels))) + geom_boxplot() +
  labs(title = "Distribution of % Whole Word Repetition Given Aphasia Group",
  y = "Log(% # of Whole Word Repetition)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(cinderella, aes(x = Group, y = sqrt(X._WWR.1), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Whole Word Repetition Given Aphasia Group",
  y = "Sqrt(% # of Whole Word Repetition)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## ------------------------------------------------------------------------------------------------
ggplot(cat, aes(x = Group, y = X._Phrase_repetitions.1, fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Phrase Repetitions Given Aphasia Group",
  y = "% # of Phase Repetitions", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(cat, aes(x = Group, y = log(X._Phrase_repetitions.1+0.01), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Phrase Repetitions Given Aphasia Group",
  y = "Log (% # of Phase Repetitions)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(cat, aes(x = Group, y = sqrt(X._Phrase_repetitions.1), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Phrase Repetitions Given Aphasia Group",
  y = "Sqrt (% # of Phase Repetitions)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## ------------------------------------------------------------------------------------------------
ggplot(cat, aes(x = Group, y = X._Word_revisions.1, fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Word Revisions Given Aphasia Group",
  y = "% # of Word Revisions", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(cat, aes(x = Group, y = log(X._Word_revisions.1+0.01), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Word Revisions Given Aphasia Group",
  y = "Log (% # of Word Revisions)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(cat, aes(x = Group, y = sqrt(X._Word_revisions.1), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Word Revisions Given Aphasia Group",
  y = "Sqrt (% # of Word Revisions)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## ------------------------------------------------------------------------------------------------
ggplot(cat, aes(x = Group, y = X._Phrase_revisions.1, fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Phrase Revisions Given Aphasia Group",
  y = "% # of Phrase Revisions", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(cat, aes(x = Group, y = log(X._Phrase_revisions.1+0.01), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Phrase Revisions Given Aphasia Group",
  y = "Log(% # of Phrase Revisions)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(cat, aes(x = Group, y = sqrt(X._Phrase_revisions.1), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Phrase Revisions Given Aphasia Group",
  y = "Sqrt(% # of Phrase Revisions)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## ------------------------------------------------------------------------------------------------
ggplot(cat, aes(x = Group, y = X._Phonological_fragment.1, fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Phonological Fragment Given Aphasia Group",
  y = "% # of Phonological Fragment", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(cat, aes(x = Group, y = log(X._Phonological_fragment.1+0.01), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Phonological Fragment Given Aphasia Group",
  y = "Log(% # of Phonological Fragment)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(cat, aes(x = Group, y = sqrt(X._Phonological_fragment.1), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Phonological Fragment Given Aphasia Group",
  y = "Sqrt(% # of Phonological Fragment)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## ------------------------------------------------------------------------------------------------
ggplot(cat, aes(x = Group, y = X._Filled_pauses.1, fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Filled Pauses Given Aphasia Group",
  y = "% # of Filled Pauses", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(cat, aes(x = Group, y = log(X._Filled_pauses.1+0.01), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Filled Pauses Given Aphasia Group",
  y = "Log(% # of Filled Pauses)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(cat, aes(x = Group, y = sqrt(X._Filled_pauses.1), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Filled Pauses Given Aphasia Group",
  y = "Sqrt(% # of Filled Pauses)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## ------------------------------------------------------------------------------------------------
ggplot(cat, aes(x = Group, y = X._WWR.1, fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Whole Word Repetition Given Aphasia Group",
  y = "% # of Whole Word Repetition", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(cat, aes(x = Group, y = log(X._WWR.1+0.01), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Whole Word Repetition Given Aphasia Group",
  y = "Log(% # of Whole Word Repetition)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(cat, aes(x = Group, y = sqrt(X._WWR.1), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Whole Word Repetition Given Aphasia Group",
  y = "Sqrt(% # of Whole Word Repetition)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## ------------------------------------------------------------------------------------------------
ggplot(sandwich, aes(x = Group, y = X._Phrase_repetitions.1, fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Phrase Repetitions Given Aphasia Group",
  y = "% # of Phase Repetitions", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(sandwich, aes(x = Group, y = log(X._Phrase_repetitions.1+0.01), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Phrase Repetitions Given Aphasia Group",
  y = "Log (% # of Phase Repetitions)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(sandwich, aes(x = Group, y = sqrt(X._Phrase_repetitions.1), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Phrase Repetitions Given Aphasia Group",
  y = "Sqrt (% # of Phase Repetitions)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## ------------------------------------------------------------------------------------------------
ggplot(sandwich, aes(x = Group, y = X._Word_revisions.1, fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Word Revisions Given Aphasia Group",
  y = "% # of Word Revisions", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(sandwich, aes(x = Group, y = log(X._Word_revisions.1+0.01), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Word Revisions Given Aphasia Group",
  y = "Log (% # of Word Revisions)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(sandwich, aes(x = Group, y = sqrt(X._Word_revisions.1), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Word Revisions Given Aphasia Group",
  y = "Sqrt (% # of Word Revisions)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## ------------------------------------------------------------------------------------------------
ggplot(sandwich, aes(x = Group, y = X._Phrase_revisions.1, fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Phrase Revisions Given Aphasia Group",
  y = "% # of Phrase Revisions", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(sandwich, aes(x = Group, y = log(X._Phrase_revisions.1+0.01), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Phrase Revisions Given Aphasia Group",
  y = "Log(% # of Phrase Revisions)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(sandwich, aes(x = Group, y = sqrt(X._Phrase_revisions.1), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Phrase Revisions Given Aphasia Group",
  y = "Sqrt(% # of Phrase Revisions)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## ------------------------------------------------------------------------------------------------
ggplot(sandwich, aes(x = Group, y = X._Phonological_fragment.1, fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Phonological Fragment Given Aphasia Group",
  y = "% # of Phonological Fragment", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(sandwich, aes(x = Group, y = log(X._Phonological_fragment.1+0.01), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Phonological Fragment Given Aphasia Group",
  y = "Log(% # of Phonological Fragment)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(sandwich, aes(x = Group, y = sqrt(X._Phonological_fragment.1), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Phonological Fragment Given Aphasia Group",
  y = "Sqrt(% # of Phonological Fragment)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## ------------------------------------------------------------------------------------------------
ggplot(sandwich, aes(x = Group, y = X._Filled_pauses.1, fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Filled Pauses Given Aphasia Group",
  y = "% # of Filled Pauses", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(sandwich, aes(x = Group, y = log(X._Filled_pauses.1+0.01), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Filled Pauses Given Aphasia Group",
  y = "Log(% # of Filled Pauses)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(sandwich, aes(x = Group, y = sqrt(X._Filled_pauses.1), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Filled Pauses Given Aphasia Group",
  y = "Sqrt(% # of Filled Pauses)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## ------------------------------------------------------------------------------------------------
ggplot(sandwich, aes(x = Group, y = X._WWR.1, fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Whole Word Repetition Given Aphasia Group",
  y = "% # of Whole Word Repetition", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(sandwich, aes(x = Group, y = log(X._WWR.1+0.01), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Whole Word Repetition Given Aphasia Group",
  y = "Log(% # of Whole Word Repetition)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(sandwich, aes(x = Group, y = sqrt(X._WWR.1), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Whole Word Repetition Given Aphasia Group",
  y = "Sqrt(% # of Whole Word Repetition)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## ------------------------------------------------------------------------------------------------
ggplot(stroke, aes(x = Group, y = X._Phrase_repetitions.1, fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Phrase Repetitions Given Aphasia Group",
  y = "% # of Phase Repetitions", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(stroke, aes(x = Group, y = log(X._Phrase_repetitions.1), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Phrase Repetitions Given Aphasia Group",
  y = "Log (% # of Phase Repetitions)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(stroke, aes(x = Group, y = sqrt(X._Phrase_repetitions), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Phrase Repetitions Given Aphasia Group",
  y = "Sqrt (% # of Phase Repetitions)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## ------------------------------------------------------------------------------------------------
ggplot(stroke, aes(x = Group, y = X._Word_revisions, fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of Word Revisions Given Aphasia Group",
  y = "# of Word Revisions", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(stroke, aes(x = Group, y = log(X._Word_revisions), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of Word Revisions Given Aphasia Group",
  y = "Log (# of Word Revisions)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(stroke, aes(x = Group, y = sqrt(X._Word_revisions), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of Word Revisions Given Aphasia Group",
  y = "Sqrt (# of Word Revisions)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## ------------------------------------------------------------------------------------------------
ggplot(stroke, aes(x = Group, y = X._Phrase_revisions, fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of Phrase Revisions Given Aphasia Group",
  y = "# of Phrase Revisions", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(stroke, aes(x = Group, y = log(X._Phrase_revisions), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of Phrase Revisions Given Aphasia Group",
  y = "Log(# of Phrase Revisions)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(stroke, aes(x = Group, y = sqrt(X._Phrase_revisions), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of Phrase Revisions Given Aphasia Group",
  y = "Sqrt(# of Phrase Revisions)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## ------------------------------------------------------------------------------------------------
ggplot(stroke, aes(x = Group, y = X._Phonological_fragment, fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of Phonological Fragment Given Aphasia Group",
  y = "# of Phonological Fragment", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(stroke, aes(x = Group, y = log(X._Phonological_fragment), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of Phonological Fragment Given Aphasia Group",
  y = "Log(# of Phonological Fragment)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(stroke, aes(x = Group, y = sqrt(X._Phonological_fragment), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of Phonological Fragment Given Aphasia Group",
  y = "Sqrt(# of Phonological Fragment)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## ------------------------------------------------------------------------------------------------
ggplot(stroke, aes(x = Group, y = X._Filled_pauses, fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of Filled Pauses Given Aphasia Group",
  y = "# of Filled Pauses", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(stroke, aes(x = Group, y = log(X._Filled_pauses), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of Filled Pauses Given Aphasia Group",
  y = "Log(# of Filled Pauses)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(stroke, aes(x = Group, y = sqrt(X._Filled_pauses), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of Filled Pauses Given Aphasia Group",
  y = "Sqrt(# of Filled Pauses)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## ------------------------------------------------------------------------------------------------
ggplot(stroke, aes(x = Group, y = X._WWR, fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of Whole Word Repetition Given Aphasia Group",
  y = "# of Whole Word Repetition", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(stroke, aes(x = Group, y = log(X._WWR), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of Whole Word Repetition Given Aphasia Group",
  y = "Log(# of Whole Word Repetition)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(stroke, aes(x = Group, y = sqrt(X._WWR), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of Whole Word Repetition Given Aphasia Group",
  y = "Sqrt(# of Whole Word Repetition)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## ------------------------------------------------------------------------------------------------
ggplot(illness, aes(x = Group, y = X._Phrase_repetitions, fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of Phrase Repetitions Given Aphasia Group",
  y = "# of Phase Repetitions", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(illness, aes(x = Group, y = log(X._Phrase_repetitions), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of Phrase Repetitions Given Aphasia Group",
  y = "Log (# of Phase Repetitions)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(illness, aes(x = Group, y = sqrt(X._Phrase_repetitions), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of Phrase Repetitions Given Aphasia Group",
  y = "Sqrt (# of Phase Repetitions)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## ------------------------------------------------------------------------------------------------
ggplot(illness, aes(x = Group, y = X._Word_revisions, fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of Word Revisions Given Aphasia Group",
  y = "# of Word Revisions", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(illness, aes(x = Group, y = log(X._Word_revisions), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of Word Revisions Given Aphasia Group",
  y = "Log (# of Word Revisions)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(illness, aes(x = Group, y = sqrt(X._Word_revisions), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of Word Revisions Given Aphasia Group",
  y = "Sqrt (# of Word Revisions)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## ------------------------------------------------------------------------------------------------
ggplot(illness, aes(x = Group, y = X._Phrase_revisions, fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of Phrase Revisions Given Aphasia Group",
  y = "# of Phrase Revisions", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(illness, aes(x = Group, y = log(X._Phrase_revisions), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of Phrase Revisions Given Aphasia Group",
  y = "Log(# of Phrase Revisions)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(illness, aes(x = Group, y = sqrt(X._Phrase_revisions), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of Phrase Revisions Given Aphasia Group",
  y = "Sqrt(# of Phrase Revisions)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## ------------------------------------------------------------------------------------------------
ggplot(illness, aes(x = Group, y = X._Phonological_fragment, fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of Phonological Fragment Given Aphasia Group",
  y = "# of Phonological Fragment", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(illness, aes(x = Group, y = log(X._Phonological_fragment), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of Phonological Fragment Given Aphasia Group",
  y = "Log(# of Phonological Fragment)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(illness, aes(x = Group, y = sqrt(X._Phonological_fragment), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of Phonological Fragment Given Aphasia Group",
  y = "Sqrt(# of Phonological Fragment)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## ------------------------------------------------------------------------------------------------
ggplot(illness, aes(x = Group, y = X._Filled_pauses, fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of Filled Pauses Given Aphasia Group",
  y = "# of Filled Pauses", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(illness, aes(x = Group, y = log(X._Filled_pauses), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of Filled Pauses Given Aphasia Group",
  y = "Log(# of Filled Pauses)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(illness, aes(x = Group, y = sqrt(X._Filled_pauses), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of Filled Pauses Given Aphasia Group",
  y = "Sqrt(# of Filled Pauses)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## ------------------------------------------------------------------------------------------------
ggplot(illness, aes(x = Group, y = X._WWR, fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of Whole Word Repetition Given Aphasia Group",
  y = "# of Whole Word Repetition", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(illness, aes(x = Group, y = log(X._WWR), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of Whole Word Repetition Given Aphasia Group",
  y = "Log(# of Whole Word Repetition)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(illness, aes(x = Group, y = sqrt(X._WWR), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of Whole Word Repetition Given Aphasia Group",
  y = "Sqrt(# of Whole Word Repetition)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## ------------------------------------------------------------------------------------------------
ggplot(stroke_illness, aes(x = Group, y = X._Phrase_repetitions.1, fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Phrase Repetitions Given Aphasia Group",
  y = "% # of Phase Repetitions", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(stroke_illness, aes(x = Group, y = log(X._Phrase_repetitions.1+0.01), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Phrase Repetitions Given Aphasia Group",
  y = "Log (% # of Phase Repetitions)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(stroke_illness, aes(x = Group, y = sqrt(X._Phrase_repetitions.1), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Phrase Repetitions Given Aphasia Group",
  y = "Sqrt (% # of Phase Repetitions)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## ------------------------------------------------------------------------------------------------
ggplot(stroke_illness, aes(x = Group, y = X._Word_revisions.1, fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Word Revisions Given Aphasia Group",
  y = "% # of Word Revisions", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(stroke_illness, aes(x = Group, y = log(X._Word_revisions.1+0.01), fill = Group)) + 
  geom_boxplot() + labs(title = "Distribution of % Word Revisions Given Aphasia Group",
  y = "Log (% # of Word Revisions)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(stroke_illness, aes(x = Group, y = sqrt(X._Word_revisions.1), fill = Group)) + 
  geom_boxplot() + labs(title = "Distribution of % Word Revisions Given Aphasia Group",
  y = "Sqrt (% # of Word Revisions)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## ------------------------------------------------------------------------------------------------
ggplot(stroke_illness, aes(x = Group, y = X._Phrase_revisions.1, fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Phrase Revisions Given Aphasia Group",
  y = "% # of Phrase Revisions", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(stroke_illness, aes(x = Group, y = log(X._Phrase_revisions.1+0.01), fill = Group)) +
  geom_boxplot() + labs(title = "Distribution of % Phrase Revisions Given Aphasia Group",
  y = "Log(% # of Phrase Revisions)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(stroke_illness, aes(x = Group, y = sqrt(X._Phrase_revisions.1), fill = Group)) + 
  geom_boxplot() + labs(title = "Distribution of % Phrase Revisions Given Aphasia Group",
  y = "Sqrt(% # of Phrase Revisions)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## ------------------------------------------------------------------------------------------------
ggplot(stroke_illness, aes(x = Group, y = X._Phonological_fragment.1, fill = Group)) + 
  geom_boxplot() + labs(title = "Distribution of % Phonological Fragment Given Aphasia Group",
  y = "% # of Phonological Fragment", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(stroke_illness, aes(x = Group, y = log(X._Phonological_fragment.1+0.01), fill = Group)) + 
  geom_boxplot() + labs(title = "Distribution of % Phonological Fragment Given Aphasia Group",
  y = "Log(% # of Phonological Fragment)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(stroke_illness, aes(x = Group, y = sqrt(X._Phonological_fragment.1), fill = Group)) + 
  geom_boxplot() + labs(title = "Distribution of % Phonological Fragment Given Aphasia Group",
  y = "Sqrt(% # of Phonological Fragment)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## ------------------------------------------------------------------------------------------------
ggplot(stroke_illness, aes(x = Group, y = X._Filled_pauses.1, fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Filled Pauses Given Aphasia Group",
  y = "% # of Filled Pauses", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(stroke_illness, aes(x = Group, y = log(X._Filled_pauses.1+0.01), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Filled Pauses Given Aphasia Group",
  y = "Log(% # of Filled Pauses)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(stroke_illness, aes(x = Group, y = sqrt(X._Filled_pauses.1), fill = Group)) + 
  geom_boxplot() + labs(title = "Distribution of % Filled Pauses Given Aphasia Group",
  y = "Sqrt(% # of Filled Pauses)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## ------------------------------------------------------------------------------------------------
ggplot(stroke_illness, aes(x = Group, y = X._WWR.1, fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Whole Word Repetition Given Aphasia Group",
  y = "% # of Whole Word Repetition", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(stroke_illness, aes(x = Group, y = log(X._WWR.1+0.01), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Whole Word Repetition Given Aphasia Group",
  y = "Log(% # of Whole Word Repetition)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(stroke_illness, aes(x = Group, y = sqrt(X._WWR.1), fill = Group)) + geom_boxplot() +
  labs(title = "Distribution of % Whole Word Repetition Given Aphasia Group",
  y = "Sqrt(% # of Whole Word Repetition)", x = "Group", fill = "Group") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## ------------------------------------------------------------------------------------------------
variables = c("Age.Month.", "Custom_field", "mor_Utts", "mor_Words", "words_min", "X._WWR.1", "X._Phonological_fragment.1", "X._Phrase_repetitions.1", "X._Word_revisions.1", "X._Phrase_revisions.1", "X._Filled_pauses.1", "IW_Dur.Utt_dur", "No_Switch_Dur.._No_Switch", "Group")
c_sub = subset(cinderella, select = variables)
colnames(c_sub) = c("Age", "AQ_score", "mor_Utts", "mor_Words", "words_min", "Whole_Word_Repetition_percent", "Phonological_fragment_percent", "Phrase_repetitions_percent", "Word_revisions_percent", "Phrase_revisions_percent", "Filled_pauses_percent", "IW_Dur_Utt_Dur", "No_Switch_Dur_Num_No_Switch", "Group")
corr_sub = na.omit(c_sub) # remove rows with NA values
corr_sub = corr_sub[(3:14)]

sub = corr_sub
sub[,1:11] = log(sub[,1:11]+0.01) # log transform + constant to the values
sub # with Group
#corr_matrix <- cor(sub[,1:11], use = "complete.obs")
data.pca <- princomp(scale(sub[,1:11], scale = TRUE, center = TRUE))
summary(data.pca)


## ------------------------------------------------------------------------------------------------
nrow(sub)
table(sub$Group)


## ------------------------------------------------------------------------------------------------
#data.pca$loadings[,1:2] # correlation between each variable and each principal component
library(knitr) # load the knitr package for table formatting
# PC1 vs PC2
loadings_df = as.data.frame(round(data.pca$loadings[,1:2], 2))
rownames(loadings_df) = c("Log Number of Utterances", "Log Number of Words", "Log Words Per Minute", 
                          "Log % Whole Word Repetition", "Log % Phonological Fragment", "Log % Phrase Repetitions", 
                          "Log % Word Revisions", "Log % Phrase Revisions", "Log % Filled Pauses", 
                          "Log Internal Utterance Pause Duration", "Log Between Utterance Pause Duration")
kable(loadings_df, format = "markdown", col.names = c("PC1", "PC2"))

# PC1 vs PC3
loadings_df = as.data.frame(round(data.pca$loadings[,c(1,3)], 2))
rownames(loadings_df) = c("Log Number of Utterances", "Log Number of Words", "Log Words Per Minute", 
                          "Log % Whole Word Repetition", "Log % Phonological Fragment", "Log % Phrase Repetitions", 
                          "Log % Word Revisions", "Log % Phrase Revisions", "Log % Filled Pauses", 
                          "Log Internal Utterance Pause Duration", "Log Between Utterance Pause Duration")
kable(loadings_df, format = "markdown", col.names = c("PC1", "PC3"))



## ------------------------------------------------------------------------------------------------
library(ggfortify)
library(plotly)
library("factoextra")
# for PC1 vs PC2
plot(data.pca, type="l")
#fviz_pca_biplot(data.pca, repel = TRUE)
fviz_pca_var(data.pca, col.var = "black", axes = c(1,2))
fviz_pca_ind(data.pca, axes = c(1,2))

# for PC1 vs PC3
plot(data.pca, type="l")
#fviz_pca_biplot(data.pca, repel = TRUE)
fviz_pca_var(data.pca, col.var = "black", axes = c(1,3))
fviz_pca_ind(data.pca, axes = c(1,3))


## ------------------------------------------------------------------------------------------------
# PC1 vs PC2
p = autoplot(prcomp(sub[,1:11], center = TRUE, scale = TRUE), data=sub, colour="Group")
ggplotly(p)
# projected = projected points from pcs
projected = prcomp(sub[,1:11], center = TRUE, scale = TRUE)$x[,c(1,2)]

# PC1 vs PC3
p = autoplot(prcomp(sub[,1:11], center = TRUE, scale = TRUE), data=sub, colour="Group", x=1, y=3)
ggplotly(p)
# projected = projected points from pcs
projected2 = prcomp(sub[,1:11], center = TRUE, scale = TRUE)$x[,c(1,3)]


projected = prcomp(sub[,1:11], center = TRUE, scale = TRUE)$x[,c(1,2)]


## ------------------------------------------------------------------------------------------------
library(mclust)
projected = data.frame(projected)
# G specifies the num of groups we want, fit GMM
fit = Mclust(data.frame(projected)[,c(1,2)])
# Plot GMM with no aphasia group labels, does clustering by itself by max probability
#plot(fit, what = "classification")
fviz_mclust(fit, "classification", geom = "point")
# Plot the GMM with aphasia group labels, actual labels
fit$classification = sub$Group
plot(fit, what = "classification")
fviz_mclust(fit, "classification", geom = "point")
#legend("topright", legend = fit$classification)


## ------------------------------------------------------------------------------------------------
# Plot the GMM with aphasia group labels, actual labels
col = c("red","blue","orange","purple","brown","green")
BICs = unlist(lapply(1:6, function(GVal){
  fit = Mclust(data.frame(projected)[,c(1,2)], G =GVal)
  fit$classification = sub$Group
  plot(fit, what = "classification", col=col)
  legend("bottomright", c("Anomic", "Broca", "Conduction", "control", "NotAphasicByWAB", "Wernicke"),
     col = col, pch = "-", trace=TRUE, cex=0.7)
  return(fit$bic)
}))

plot(1:6, BICs, main="BIC Screeplot", xlab="Num of Clusters", pch=15, col="blue")
lines(1:6, BICs)


## ------------------------------------------------------------------------------------------------
# Just for prediction, see which observation belongs to which aphasia group (by probability)
fit2 = MclustDA(sub[,1:11], class = sub$Group)
#plot(fit2, what = "classification")
pred = predict(fit2, newdata = sub[,1:11])
clust_summary = summary(fit2, parameters = TRUE)
# tab gives the confusion matrix
clust_summary$tab
# ce gives classification error (training error)
clust_summary$ce


## ------------------------------------------------------------------------------------------------
sub$id = seq_along(c(1:nrow(sub)))
predict(fit2, newdata = sub[8,1:11])


## ------------------------------------------------------------------------------------------------
variables = c("Age.Month.", "Custom_field", "mor_Utts", "mor_Words", "words_min", "X._Phonological_fragment.1", "X._Phrase_repetitions.1", "X._Word_revisions.1", "X._Phrase_revisions.1", "X._Filled_pauses.1", "IW_Dur.Utt_dur", "No_Switch_Dur.._No_Switch", "Group")
c_sub = subset(cat, select = variables)
colnames(c_sub) = c("Age", "AQ_score", "mor_Utts", "mor_Words", "words_min", "Phonological_fragment_percent", "Phrase_repetitions_percent", "Word_revisions_percent", "Phrase_revisions_percent", "Filled_pauses_percent", "IW_Dur_Utt_Dur", "No_Switch_Dur_Num_No_Switch", "Group")
corr_sub = na.omit(c_sub) # remove rows with NA values
corr_sub = corr_sub[(3:13)]
sub = corr_sub
sub[,1:10] = log(sub[,1:10]+0.01) # log transform + constant to the values
sub # with Group
corr_matrix <- cor(sub[,1:10], use = "complete.obs")
data.pca <- princomp(scale(sub[,1:10], scale = TRUE, center = TRUE))
summary(data.pca)


## ------------------------------------------------------------------------------------------------
data.pca$loadings[,1:2] # correlation between each variable and each principal component


## ------------------------------------------------------------------------------------------------
library(ggfortify)
library(plotly)
library("factoextra")
#biplot(data.pca)
plot(data.pca, type="l")
fviz_pca_biplot(data.pca, repel = TRUE)
fviz_pca_var(data.pca, col.var = "black")


## ------------------------------------------------------------------------------------------------
p = autoplot(prcomp(sub[,1:10], center = TRUE, scale = TRUE), data=sub, colour="Group")
ggplotly(p)
p$sub[,1:10][[1]]$x
projected = prcomp(sub[,1:10], center = TRUE, scale = TRUE)$x


## ------------------------------------------------------------------------------------------------
# corr_sub not log transformed, raw data
groups = c("Anomic", "Broca", "Conduction", "control", "NotAphasicByWAB", "Wernicke")
variables = c("mor_Utts", "mor_Words", "words_min", "Phonological_fragment_percent", "Phrase_repetitions_percent", "Word_revisions_percent", "Phrase_revisions_percent", "Filled_pauses_percent", "IW_Dur_Utt_Dur", "No_Switch_Dur_Num_No_Switch")
df = data.frame(matrix(nrow=10, ncol=6))
colnames(df) = groups; rownames(df) = variables
for (j in c(1:6)){
  n = sum(corr_sub$Group == groups[j])
  for (i in c(1:10)){
    # round to 3 decimal digits
    df[i,j] = round(sum(corr_sub[,i] == 0 & corr_sub$Group == groups[j])/n, 3)
  }
}
df


## ------------------------------------------------------------------------------------------------
anova_pr = aov(log(X._Phrase_repetitions.1+0.01) ~ factor(Group, levels = levels), data = cinderella)
summary(anova_pr)


## ------------------------------------------------------------------------------------------------
TukeyHSD(anova_pr, order=TRUE)
plot(TukeyHSD(anova_pr), las = 1, cex.axis = 0.3)


## ------------------------------------------------------------------------------------------------
anova_wr = aov(log(X._Word_revisions.1+0.01) ~ Group, data = cinderella)
summary(anova_wr)


## ------------------------------------------------------------------------------------------------
TukeyHSD(anova_wr, order=TRUE)
plot(TukeyHSD(anova_wr), las = 1, cex.axis = 0.3)


## ------------------------------------------------------------------------------------------------
anova_prev = aov(log(X._Phrase_revisions.1+0.01) ~ Group, data = cinderella)
summary(anova_prev)


## ------------------------------------------------------------------------------------------------
TukeyHSD(anova_prev, order=TRUE)
plot(TukeyHSD(anova_prev), las = 1, cex.axis = 0.3)


## ------------------------------------------------------------------------------------------------
anova_pf = aov(log(X._Phonological_fragment.1+0.01) ~ Group, data = cinderella)
summary(anova_pf)


## ------------------------------------------------------------------------------------------------
TukeyHSD(anova_pf, order=TRUE)
plot(TukeyHSD(anova_pf), las = 1, cex.axis = 0.3)


## ------------------------------------------------------------------------------------------------
anova_fp = aov(log(X._Filled_pauses.1+0.01) ~ Group, data = cinderella)
summary(anova_fp)


## ------------------------------------------------------------------------------------------------
TukeyHSD(anova_fp, order=TRUE)
plot(TukeyHSD(anova_fp), las = 1, cex.axis = 0.3)


## ------------------------------------------------------------------------------------------------
anova_wwr = aov(log(X._WWR.1+0.01) ~ Group, data = cinderella)
summary(anova_wwr)


## ------------------------------------------------------------------------------------------------
TukeyHSD(anova_wwr, order=TRUE)
plot(TukeyHSD(anova_wwr), las = 1, cex.axis = 0.3)


## ------------------------------------------------------------------------------------------------
anova_iupd = aov(log(IW_Dur.Utt_dur+0.01) ~ Group, data = cinderella)
summary(anova_iupd)


## ------------------------------------------------------------------------------------------------
TukeyHSD(anova_iupd, order=TRUE)
plot(TukeyHSD(anova_iupd), las = 1, cex.axis = 0.3)


## ------------------------------------------------------------------------------------------------
anova_nsdns = aov(log(No_Switch_Dur.._No_Switch+0.01) ~ Group, data = cinderella)
summary(anova_nsdns)


## ------------------------------------------------------------------------------------------------
TukeyHSD(anova_nsdns, order=TRUE)
plot(TukeyHSD(anova_nsdns), las = 1, cex.axis = 0.3)

