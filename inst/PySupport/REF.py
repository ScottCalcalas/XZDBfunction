import os

folder = os.path.dirname(os.path.abspath(__file__))

for filename in os.listdir(folder):
    filepath = os.path.join(folder, filename)
    if os.path.isfile(filepath):
        ext = os.path.splitext(filename)[1]
        print(f"{filename}: {ext if ext else 'No extension'}")