import sys
import numpy as np

line_separator = '-----'
segment = sorted([float(sys.argv[1]), float(sys.argv[2])])
print(segment)
segment_distance = float(sys.argv[3])

subsegments = np.arange(segment[0] + (5 - segment[0] % 5) % 5, np.ceil(segment[1]), 5).tolist()

if len(subsegments) == 0:
    raise ValueError("No subsegments found")

segment_points = sorted([*segment, *subsegments])


for i in range(1, len(segment_points)):
    print(segment_points[i-1], end=line_separator)
    
    map_distance = (segment_points[i] - segment_points[i-1]) / (segment[1] - segment[0]) * segment_distance
    print(map_distance, end=line_separator)

print(segment_points[-1])