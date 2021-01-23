
class AsteroidBelt:
    def __init__(filepath="Day10Input.txt"):
        with open(filepath, "r") as file:
            content = file.read().splitlines()

        self.width = len(content.get(0))
        self.height = len(content)

        asteroid_locations = []
        i = 0
        j = 0
        for line in content:
            for char in line:
                if char is '#':
                    asteroid_locations += (i, j)
                j += 1
            i += 1
        self.belt = Set(asteroid_locations)

    def all_locations(self):
        for i in range(0, self.width)

    def around_point(point: Point):
        pass
