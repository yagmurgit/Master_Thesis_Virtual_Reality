using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System;
using System.IO;

public class TrajectoryAnalysis : MonoBehaviour
{
    public string filePath = @"C:\Users\ryagm\Downloads\multifloor_trajectory.csv"; // Path to the input CSV
    public GameObject[] boxes;
    private Dictionary<int, float> participantDistances = new Dictionary<int, float>();

    // Start is called before the first frame update
    void Start()
    {
        var boxesKeyValue = new Dictionary<int, GameObject>();
        foreach (var box in boxes)
        {
            boxesKeyValue.Add(box.GetComponent<BoxInfo>().Id, box);
        }

        try
        {
            using (StreamReader reader = new StreamReader(filePath))
            {
                string line;
                var routeLines = new GameObject("routeLines ");
                var participantId = 0;

                // Reading each line (row) in the CSV file
                while ((line = reader.ReadLine()) != null)
                {
                    // Split the row into columns based on a comma
                    string[] columns = line.Split(',');
                    var route = new List<int>();

                    for (int i = 2; i < columns.Length; i++)
                    {
                        var boxId = 0;
                        var column = columns[i];

                        if (column[0] == '"')
                        {
                            boxId = int.Parse(column.Substring(1));
                        }
                        else if (column == "Route")
                        {
                            continue;
                        }
                        else if (column[column.Length - 1] == '"')
                        {
                            boxId = int.Parse(column.Substring(0, column.Length - 1));
                        }
                        else
                        {
                            boxId = int.Parse(column);
                        }

                        route.Add(boxId);
                    }

                    var participantRoute = new GameObject("route " + participantId);
                    participantRoute.transform.parent = routeLines.transform;

                    var totalDistance = 0f;
                    for (var i = 0; i < route.Count - 1; i++)
                    {
                        var lineSegment = new GameObject("line " + i);
                        var lineRenderer1 = lineSegment.AddComponent<LineRenderer>();
                        lineRenderer1.startWidth = 0.1f;
                        lineRenderer1.endWidth = 0.5f;
                        lineRenderer1.material = new Material(Shader.Find("Sprites/Default"));

                        var startingPoint = boxesKeyValue[route[i]].transform.position;
                        var endPoint = boxesKeyValue[route[i + 1]].transform.position;

                        Debug.Log($"Start Point: {startingPoint}, End Point: {endPoint}");

                        var startNavMeshPoint = GetClosestPointOnNavMesh(startingPoint);
                        var endNavMeshPoint = GetClosestPointOnNavMesh(endPoint);
                        Debug.Log($"Start NavMesh Point: {startNavMeshPoint}, End NavMesh Point: {endNavMeshPoint}");

                        totalDistance += CalculateAndVisualizePath(startingPoint, endPoint, lineRenderer1);
                        lineSegment.transform.parent = participantRoute.transform;
                    }

                    // Log distance and store it in the dictionary
                    Debug.Log("Calculated distance for participant with ID: " + participantId + " is " + totalDistance + " meters.");
                    participantDistances[participantId] = totalDistance;

                    participantId++;
                }

                // Write distances to CSV
                WriteDistancesToCsv();
            }
        }
        catch (Exception e)
        {
            Debug.Log("Error: " + e.Message);
        }
    }

    float CalculateAndVisualizePath(Vector3 startPoint, Vector3 endPoint, LineRenderer lineRenderer)
    {
        // Find the closest points on the NavMesh
        Vector3 startNavMeshPoint = GetClosestPointOnNavMesh(startPoint);
        Vector3 endNavMeshPoint = GetClosestPointOnNavMesh(endPoint);

        // Calculate the path
        UnityEngine.AI.NavMeshPath path = new UnityEngine.AI.NavMeshPath();
        if (!UnityEngine.AI.NavMesh.CalculatePath(startNavMeshPoint, endNavMeshPoint, UnityEngine.AI.NavMesh.AllAreas, path))
        {
            Debug.LogError($"Failed to calculate path from {startNavMeshPoint} to {endNavMeshPoint}");
            return 0f;
        }

        Debug.Log($"Path Corners Count: {path.corners.Length}");

        // Draw the path using a LineRenderer
        DrawPath(path, lineRenderer);

        return CalculatePathDistance(path);
    }

    Vector3 GetClosestPointOnNavMesh(Vector3 position)
    {
        UnityEngine.AI.NavMeshHit hit;
        if (UnityEngine.AI.NavMesh.SamplePosition(position, out hit, 1.0f, UnityEngine.AI.NavMesh.AllAreas))
        {
            return hit.position;
        }
        return position;  // Fallback if no point found (shouldn't happen if NavMesh is set up properly)
    }

    void DrawPath(UnityEngine.AI.NavMeshPath path, LineRenderer lineRenderer)
    {
        if (lineRenderer != null && path.corners.Length > 0)
        {
            lineRenderer.positionCount = path.corners.Length;
            lineRenderer.SetPositions(path.corners);
            Debug.Log("Path drawn with " + path.corners.Length + " corners.");
        }
    }

    float CalculatePathDistance(UnityEngine.AI.NavMeshPath path)
    {
        if (path.corners.Length < 2)
        {
            return 0f; // No path or path with only one point
        }

        float totalDistance = 0f;

        for (int i = 0; i < path.corners.Length - 1; i++)
        {
            totalDistance += Vector3.Distance(path.corners[i], path.corners[i + 1]);
        }

        return totalDistance;
    }

    void WriteDistancesToCsv()
    {
        string outputPath = @"C:\Users\ryagm\Downloads\distances_multifloor-floor.csv"; // Updated file name
        using (StreamWriter writer = new StreamWriter(outputPath))
        {
            // Write the header
            writer.WriteLine("ParticipantID,TotalDistance");

            // Iterate through each participant's data
            foreach (var entry in participantDistances)
            {
               writer.WriteLine($"{entry.Key},{entry.Value}");
            }
        }

        Debug.Log("Distances saved to " + outputPath);
    }
}
