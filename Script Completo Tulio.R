ThulepDF <- data.frame(THULEP1)
# Distribución de Edades
hist(ThulepDF$AGE, main = "Distribución de Edades", xlab = "Edad", ylab = "", col = "skyblue", border = "black")
# Get the frequency of surgical techniques
technique_frequencies <- colSums(ThulepDF[, c("Tecnica.Trilobar", "Tecnica.Mushroom", "Tecnica.Tangerin", "Fotovaporizacion", "Tecnica.3.puntos")])

# Increase the width of the graphical device and adjust font size
options(repr.plot.width=8, repr.plot.height=5)  # Adjust width and height as needed
par(cex.axis = 0.8)

# Bar plot with manually provided labels
barplot(technique_frequencies, 
        main = "Frecuencia de Técnicas Quirúrgicas", 
        xlab = "", 
        ylab = "Frecuencia", 
        col = "lightgreen",
        border = "black",
        ylim = c(0, max(technique_frequencies) + 5),
        names.arg = c("Trilobar", "Mushroom", "Tangerin", "Fotovaporizacion", "3 puntos"),
        las = 2)  # Rotate x-axis labels

# Agregar más números en el eje Y
axis(2, at = seq(0, max(colSums(ThulepDF[, c("Tecnica.Trilobar", "Tecnica.Mushroom", "Tecnica.Tangerin", "Fotovaporizacion", "Tecnica.3.puntos")])) + 5, by = 5))

# Diagrama de Caja para la Duración del Procedimiento
boxplot(ThulepDF$`Duración.del.procedimiento..Minutos.`, 
        main = "Diagrama de Caja de Duración del Procedimiento", 
        ylab = "Duración (Minutos)", 
        col = "lightcoral",
        border = "black")

# Histograma para la Duración del Procedimiento
hist(ThulepDF$`Duración.del.procedimiento..Minutos.`, 
     main = "Histograma de Duración del Procedimiento", 
     xlab = "Duración (Minutos)", 
     col = "skyblue", 
     border = "black")




# Load the ggplot2 library
library(ggplot2)

# Select complication columns
complication_columns <- ThulepDF[, c("Beach.Balls", "Incontinencia.urinaria.transitoria", "Estenosis.de.uretra", "TEP", "TVP", "Hematuria.post.cirugia", "Perforación.capsular")]

# Calculate the frequency (count of 1s) for each complication
complication_counts <- colSums(complication_columns)

# Create a bar chart with ggplot2
ggplot(data.frame(complication = names(complication_counts), frequency = complication_counts), aes(x = complication, y = frequency, fill = complication)) +
  geom_bar(stat = "identity") +
  labs(title = "Frecuencia de Complicaciones",
       x = "Tipo de Complicación",
       y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        axis.ticks.y = element_blank(),  # Remove default y-axis ticks
        axis.text.y = element_text(size = 8)) +  # Adjust y-axis text size
  scale_y_continuous(breaks = seq(0, max(complication_counts), by = 1))  # Specify y-axis tick marks


# Load the ggplot2 library
library(ggplot2)

# Convert "Sangrado (ml)" to numeric
ThulepDF$Sangrado..ml. <- as.numeric(as.character(ThulepDF$Sangrado..ml.))

# Create a scatter plot for the amount of blood produced during surgery
ggplot(ThulepDF, aes(x = "", y = Sangrado..ml., color = Sangrado..ml.)) +
  geom_point() +
  labs(title = "Cantidad de Sangrado durante la Cirugía",
       x = "",
       y = "Sangrado (ml)") +
  theme_minimal()


# Load the ggplot2 library
library(ggplot2)

# Create a scatter plot for the duration of surgery
ggplot(ThulepDF, aes(x = "", y = Duración.del.procedimiento..Minutos., color = Duración.del.procedimiento..Minutos.)) +
  geom_point() +
  labs(title = "Duración del Procedimiento por Paciente",
       x = "",
       y = "Duración del Procedimiento (Minutos)") +
  theme_minimal()

# Load the ggplot2 library
library(ggplot2)

# Create a bar plot for the duration of surgery
ggplot(ThulepDF, aes(x = "", y = Duración.del.procedimiento..Minutos., fill = Duración.del.procedimiento..Minutos.)) +
  geom_bar(stat = "identity") +
  labs(title = "Duración del Procedimiento por Paciente",
       x = "",
       y = "Duración del Procedimiento (Minutos)") +
  theme_minimal()


# Load the ggplot2 library
library(ggplot2)

# Convert "Duración del Procedimiento (Minutos)" to numeric
ThulepDF$Duración.del.procedimiento..Minutos. <- as.numeric(as.character(ThulepDF$Duración.del.procedimiento..Minutos.))

# Calculate the average duration
average_duration <- mean(ThulepDF$Duración.del.procedimiento..Minutos., na.rm = TRUE)

# Create a scatter plot for the duration of surgery
ggplot(ThulepDF, aes(x = "", y = Duración.del.procedimiento..Minutos., color = Duración.del.procedimiento..Minutos.)) +
  geom_point() +
  geom_hline(yintercept = average_duration, linetype = "dashed", color = "red", size = 1) +  # Add a dashed line for the average
  labs(title = "Duración del Procedimiento por Paciente",
       x = "",
       y = "Duración del Procedimiento (Minutos)") +
  theme_minimal()



# Load the ggplot2 library
library(ggplot2)

# Convert "Duración del Procedimiento (Minutos)" to numeric
ThulepDF$Duración.del.procedimiento..Minutos. <- as.numeric(as.character(ThulepDF$Duración.del.procedimiento..Minutos.))

# Calculate the average duration
average_duration <- mean(ThulepDF$Duración.del.procedimiento..Minutos., na.rm = TRUE)

# Create a scatter plot for the duration of surgery
ggplot(ThulepDF, aes(x = "", y = Duración.del.procedimiento..Minutos., color = Duración.del.procedimiento..Minutos.)) +
  geom_point() +
  geom_hline(yintercept = average_duration, linetype = "dashed", color = "red", size = 1) +  # Add a dashed line for the average
  labs(title = "Duración del Procedimiento por Paciente",
       x = "",
       y = "Duración del Procedimiento (Minutos)",
       caption = paste("Media: ", round(average_duration, 2))) +  # Add a caption with the mean value
  theme_minimal()

# Load the ggplot2 library
library(ggplot2)

# Create a bar plot for the postmiccional test outcomes
ggplot(ThulepDF, aes(x = Test.postmiccional, fill = Test.postmiccional)) +
  geom_bar() +
  labs(title = "Resultado del Test Postmiccional",
       x = "Resultado",
       y = "Frecuencia") +
  theme_minimal()


# Load the ggplot2 library
library(ggplot2)

# Convert "Hematuria.post.cirugia" to a factor if it's not already
ThulepDF$Hematuria.post.cirugia <- factor(ThulepDF$Hematuria.post.cirugia)

# Create a bar plot for the presence of hematuria post-cirugia
ggplot(ThulepDF, aes(x = factor(Hematuria.post.cirugia), fill = factor(Hematuria.post.cirugia))) +
  geom_bar() +
  labs(title = "Presencia de Hematuria Post Cirugía",
       x = "Hematuria Post Cirugía",
       y = "Frecuencia") +
  scale_fill_manual(values = c("0" = "darkblue", "1" = "darkred"), name = "Descripción", labels = c("No tuvo hematuria", "Tuvo hematuria")) +
  theme_minimal()



