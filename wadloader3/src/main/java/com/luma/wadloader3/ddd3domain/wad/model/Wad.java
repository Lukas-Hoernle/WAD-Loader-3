package com.luma.wadloader3.ddd3domain.wad.model;

import com.luma.wadloader3.ddd3domain.files.model.FilePath;
import jakarta.persistence.*;
import lombok.*;

@Entity
@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Table(name = "Wads", uniqueConstraints = @UniqueConstraint(columnNames = "name"))
public class Wad {

    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Id
    private int id;

    @Column(name = "name")

    private String name;

    @Column(name = "description")
    private String description;

    @Column(name = "filePath")
    private FilePath filePath;
}
