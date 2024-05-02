import os
import re

def find_libraries_in_r_files(folder_path):
    library_names = set()

    # Iterate through all files in the folder
    for root, dirs, files in os.walk(folder_path):
        for file in files:
            if file.endswith('.r'):
                file_path = os.path.join(root, file)
                with open(file_path, 'r') as f:
                    content = f.readlines()
                    print (type(content))
                    for line in content:
                        if "library" in line:
                            library_names.add(line.replace("library", "").replace("(", "").replace(")", "").split()[0])
    return library_names

if __name__ == "__main__":
    folder_path = os.getcwd()
    libraries = find_libraries_in_r_files(folder_path)
    packages = []
    print("Libraries to install:")
    for lib in libraries:
        if "\"" in lib:
            packages.append(lib)
        else:
            packages.append("\"" + lib + "\"")
    print ("install.packages(" + ", ".join(packages) + ")")
