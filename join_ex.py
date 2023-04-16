import os

# Define the name of the output file
output_filename = "P2.pl"

# Loop through the IDs from 1 to 26
for i in range(1, 27):
    print("Processing ID: " + str(i))
    # Define the name of the input file for the current ID
    input_filename = f"ex{i}.pl"

    # Check if the input file exists in the current directory
    if os.path.exists(input_filename):
        # Open the input file for reading and the output file for appending
        with open(input_filename, "r", encoding="utf-8") as input_file, open(output_filename, "a", encoding="utf-8") as output_file:
            # Read the contents of the input file and write them to the output file
            output_file.write(f"%%%%%%%%%%ex{i}%%%%%%%%%%\r")
            output_file.write(input_file.read())
            output_file.write("\r")