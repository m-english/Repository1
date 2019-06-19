# titanic is avaliable in your workspace
# 1 - Check the structure of titanic to determine what data
#and data types (int, char, etc) are in the dataset.

str(titanic)

# 2 - Use ggplot() for the first instruction, pulling from the Titanic data set
#to construct a bar graph that will put the values for the passenger class on the x-axis
#and the number of  passengers, male and female, on the y-axis.
#The fill indicates the colors of the bars that distinguish between male and female.
#Finally, the assignment of "dodge" to the position in the geom_bar function sets the data on male and female side by side as two bars instead of overlaying each other on oneas a total amount (in y-value)

ggplot(titanic, aes(x = Pclass, fill = Sex)) +
  geom_bar(position = "dodge")



# 3 - Plot 2, add facet_grid() layer
#The facet_grid layer creates two separate bar graphs, distinct by the number of survivors.
ggplot(titanic, aes(x = Pclass, fill = Sex)) +
  geom_bar(position = "dodge") + facet_grid(. ~Survived)



# 4 - Define an object for position jitterdodge, to use below
#This changes the chart into a jitter graph with the dodge component; the values of each point won't overlay on the axis but will be spread out more finely.
posn.jd <- position_jitterdodge(0.5, 0, 0.6)


# 5 - Plot 3, but use the position object from instruction 4
#Adding the "alpha" component creates a certain transluence of the data points based on the value of alpha.
#The size of each point (3) based on the internal R size database.

ggplot(titanic, aes(x = Pclass, y = Age, color = Sex)) +
  geom_point(size = 3, alpha = 0.5, position = posn.jd) + facet_grid(. ~Survived)

