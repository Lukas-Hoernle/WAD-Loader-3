package com.luma.wadloader3.ddd3domain.wad.model;

import jakarta.persistence.*;
import lombok.*;

import java.util.Map;

@Entity
@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Table(name = "WadPacks")
public class WadPack {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private int id;

    @Column(name = "name")
    private String name;

    @Column(name = "description")
    private String description;

    @Column(name = "wads")
    @OneToMany
    @MapKeyColumn(name = "wad_order")
    @Builder.Default
    private Map<Integer, Wad> wads = Map.of();
}
