using System.Collections.Generic;
using UnityEngine;

public class HeatMapVisualizer : MonoBehaviour
{
    public int gridSize = 10; // Number of cells in each dimension
    public float cellSize = 1f; // Size of each cell
    public Color minColor = Color.blue; // Color for low density
    public Color maxColor = Color.red; // Color for high density

    private float[,] heatMapData;
    private Texture2D heatMapTexture;

    void Start()
    {
        heatMapData = new float[gridSize, gridSize];
        heatMapTexture = new Texture2D(gridSize, gridSize);
        GetComponent<Renderer>().material.mainTexture = heatMapTexture;
        UpdateHeatMap();
    }

    public void AddHeat(Vector3 position)
    {
        // Convert world position to grid coordinates
        int xIndex = Mathf.Clamp(Mathf.FloorToInt(position.x / cellSize), 0, gridSize - 1);
        int yIndex = Mathf.Clamp(Mathf.FloorToInt(position.z / cellSize), 0, gridSize - 1);

        // Increment the heat value at this cell
        heatMapData[xIndex, yIndex] += 1;
    }

    void UpdateHeatMap()
    {
        // Normalize and update the texture
        float maxDensity = 0;
        foreach (float value in heatMapData)
        {
            if (value > maxDensity) maxDensity = value;
        }

        for (int x = 0; x < gridSize; x++)
        {
            for (int y = 0; y < gridSize; y++)
            {
                float density = heatMapData[x, y] / maxDensity; // Normalize
                heatMapTexture.SetPixel(x, y, Color.Lerp(minColor, maxColor, density));
            }
        }

        heatMapTexture.Apply();
    }
}
