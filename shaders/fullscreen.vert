#version 450

layout(location = 0) out vec2 fragUV;

void main() {
    /* Fullscreen triangle from gl_VertexIndex — no VBO needed.
     * Vertex 0: (-1, -1), UV (0, 0)
     * Vertex 1: ( 3, -1), UV (2, 0)
     * Vertex 2: (-1,  3), UV (0, 2)
     * The rasterizer clips to the viewport. */
    fragUV = vec2((gl_VertexIndex << 1) & 2, gl_VertexIndex & 2);
    gl_Position = vec4(fragUV * 2.0 - 1.0, 0.0, 1.0);
}
